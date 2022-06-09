/***************************************************************************************
 * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2022 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package top

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.util._
import device.TLPMAIO
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink.{DevNullParams, TLError}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.jtag.JTAGIO
import freechips.rocketchip.tilelink.{TLFIFOFixer, TLToAXI4, TLWidthWidget, TLXbar}
import freechips.rocketchip.util.{ElaborationArtefacts, HasRocketChipStageUtils, Pow2ClockDivider}

class TopIOAdapter(_top: XSTop)(implicit p: Parameters) extends Module {
  // This io is the same as NANHU.
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val sram_config = Input(UInt(16.W))
    val extIntrs = Input(UInt(64.W))
    val pll0_lock = Input(Bool())
    val pll0_ctrl = Output(Vec(6, UInt(32.W)))
    val systemjtag = new Bundle {
      val jtag = Flipped(new JTAGIO(hasTRSTn = false))
      val reset = Input(AsyncReset())
      val mfr_id = Input(UInt(11.W))
      val part_number = Input(UInt(16.W))
      val version = Input(UInt(4.W))
    }
    val debug_reset = Output(Bool())
    val cacheable_check = new TLPMAIO()
  })

  // This is the IO of southlake.
  val top = IO(Flipped(_top.module.io.cloneType))

  io := DontCare

  top.clock := io.clock
  top.clock_div2 := Module(new Pow2ClockDivider(1)).io.clock_out
  top.reset := io.reset.asAsyncReset
  top.extIntrs := io.extIntrs
  top.systemjtag <> io.systemjtag
  io.debug_reset := top.debug_reset
  // soc.io.rtc_clock is a div100 of soc.io.clock
  val rtcClockDiv = 100
  val rtcTickCycle = rtcClockDiv / 2
  val rtcCounter = RegInit(0.U(log2Ceil(rtcTickCycle + 1).W))
  rtcCounter := Mux(rtcCounter === (rtcTickCycle - 1).U, 0.U, rtcCounter + 1.U)
  val rtcClock = RegInit(false.B)
  when (rtcCounter === 0.U) {
    rtcClock := ~rtcClock
  }
  top.rtc_clock := rtcClock
  top.riscv_rst_vec.foreach(_ := 0x1ffff80000L.U)

}

class TopMemoryAdapter(_top: XSTop)(implicit p: Parameters) extends Module {
  val memory = IO(_top.module.memory.cloneType)
  val top = IO(Flipped(_top.module.memory.cloneType))

  def reMapAddress(addr: UInt): UInt = {
    // Memory: 0x20_0000_0000 --> 0x8000_0000
    addr - (0x2000000000L - 0x80000000L).U
  }

  memory <> top
  memory.elts.zip(top.elts).foreach{ case (m, t) =>
    m.ar.bits.addr := reMapAddress(t.ar.bits.addr)
    m.aw.bits.addr := reMapAddress(t.aw.bits.addr)
  }
}

class TopPeripheralAdapter(_top: XSTop)(implicit p: Parameters) extends Module {

  class TopPeripheralBusWidthAdapater()(implicit p: Parameters) extends LazyModule {
    val slave = AXI4SlaveNode(_top.misc.peripheralNode.portParams.map(_.copy(beatBytes = 8)))
    val master = AXI4MasterNode(List(_top.misc.peripheralNode.in.head._2.master))

    val errorDev = LazyModule(new TLError(
      params = DevNullParams(
        address = Seq(AddressSet(0x0L, 0x7fffffffL)),
        maxAtomic = 8,
        maxTransfer = 128
      ),
      beatBytes = 8
    ))
    val tlBus = TLXbar()
    tlBus :=
      TLFIFOFixer() :=
      TLWidthWidget(32) :=
      AXI4ToTL() :=
      AXI4UserYanker(Some(1)) :=
      master
    errorDev.node := tlBus
    slave := AXI4UserYanker(Some(1)) := TLToAXI4() := tlBus

    val io_slave = InModuleBody {
      slave.makeIOs()
    }
    val io_master = InModuleBody {
      master.makeIOs()
    }
    lazy val module = new LazyModuleImp(this) { }
  }

  val l_widthAdapter = LazyModule(new TopPeripheralBusWidthAdapater)
  val widthAdapter = Module(l_widthAdapter.module)

  val peripheral = IO(l_widthAdapter.io_slave.cloneType)
  val top = IO(Flipped(_top.module.peripheral.cloneType))

  peripheral <> l_widthAdapter.io_slave
  l_widthAdapter.io_master.getWrappedValue.elts.foreach(x => dontTouch(x))
  l_widthAdapter.io_master.getWrappedValue.elts.foreach(_ := DontCare)
  l_widthAdapter.io_master <> top

  def reMapAddress(addr: UInt): UInt = {
    // Peripheral:
    // (1) UART: 0x1f_0005_0000 --> 0x4060_0000
    // (2) QSPI: 0x1f_fff8_0000 --> 0x1000_0000
    Mux(addr(31), addr - (0x1ffff80000L - 0x10000000L).U, addr - (0x1f00050000L - 0x40600000L).U)
  }
  peripheral.elts.zip(l_widthAdapter.io_slave.elts).foreach{ case (p, a) =>
    p.ar.bits.addr := reMapAddress(a.ar.bits.addr)
    p.aw.bits.addr := reMapAddress(a.aw.bits.addr)
  }
}

class FPGATop()(implicit p: Parameters) extends Module {
  // override the module name to act the same like XSTop.
  override def desiredName: String = "XSTop"

  val lazy_module_top = LazyModule(new XSTop)
  val top = Module(lazy_module_top.module)

  // Primary IO is from the adapter
  val io_adapter = Module(new TopIOAdapter(lazy_module_top))
  io_adapter.top <> top.io
  val io = IO(io_adapter.io.cloneType)
  io <> io_adapter.io

  val memory_adapter = Module(new TopMemoryAdapter(lazy_module_top))
  memory_adapter.top <> top.memory
  val memory = IO(memory_adapter.memory.cloneType)
  memory <> memory_adapter.memory

  val peripheral_adapter = Module(new TopPeripheralAdapter(lazy_module_top))
  peripheral_adapter.top <> top.peripheral
  val peripheral = IO(peripheral_adapter.peripheral.cloneType)
  peripheral <> peripheral_adapter.peripheral

  val dma = IO(Flipped(lazy_module_top.misc.dma.cloneType))
  dma <> top.dma

  // Extra bits are DontCare
  top.xsx_ultiscan_ijtag := DontCare
  top.xsl2_ultiscan_ijtag := DontCare
  top.xsx_ultiscan_uscan := DontCare
  top.xsl2_ultiscan_uscan := DontCare
  top.hd2prf_in := DontCare
  top.hsuspsr_in := DontCare
  top.l1l2_mbist_jtag := DontCare
  if (top.l3_mbist.ijtag.isDefined) {
    top.l3_mbist.ijtag.get := DontCare
  }
}


object FPGATop extends App with HasRocketChipStageUtils {
  override def main(args: Array[String]): Unit = {
    val (config, firrtlOpts) = ArgParser.parse(args)
    XiangShanStage.execute(firrtlOpts, Seq(
      ChiselGeneratorAnnotation(() => {
        DisableMonitors(p => Module(new FPGATop()(p)))(config)
      })
    ))
    ElaborationArtefacts.files.foreach{ case (extension, contents) =>
      writeOutputFile("./build", s"XSTop.${extension}", contents())
    }
  }
}
