/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
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

package xiangshan.cache.mmu

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.cache.{HasDCacheParameters, MemoryOpConstants}
import utils._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.tilelink._

/* ptw finite state machine, the actual page table walker
 */
class PtwFsmIO()(implicit p: Parameters) extends PtwBundle {
  val req = Flipped(DecoupledIO(new Bundle {
    val source = UInt(bPtwWidth.W)
    val l1Hit = Bool()
    val l2Hit = Bool()
    val vpn = UInt(vpnLen.W)
    val ppn = UInt(ppnLen.W)
  }))
  val resp = DecoupledIO(new Bundle {
    val source = UInt(bPtwWidth.W)
    val resp = new PtwResp
  })

  val mem = new Bundle {
    val req = DecoupledIO(new Bundle {
      val addr = UInt(PAddrBits.W)
    })
    val resp = Flipped(ValidIO(new Bundle {
      val data = UInt(MemBandWidth.W)
    }))
  }

  val csr = Input(new TlbCsrBundle)
  val sfence = Input(new SfenceBundle)
  val sfenceLatch = Output(Bool())
  val refill = Output(new Bundle {
    val vpn = UInt(vpnLen.W)
    val level = UInt(log2Up(Level).W)
    val memAddr = UInt(PAddrBits.W)
  })
}

class PtwFsm()(implicit p: Parameters) extends XSModule with HasPtwConst with HasColtUtils{
  val io = IO(new PtwFsmIO)

  val sfence = io.sfence
  val mem = io.mem
  val satp = io.csr.satp

  val s_idle :: s_mem_req :: s_mem_resp :: s_resp :: Nil = Enum(4)
  val state = RegInit(s_idle)
  val level = RegInit(0.U(log2Up(Level).W))
  val ppn = Reg(UInt(ppnLen.W))
  val vpn = Reg(UInt(vpnLen.W))
  val levelNext = level + 1.U

  val sfenceLatch = RegEnable(false.B, init = false.B, mem.resp.valid) // NOTE: store sfence to disable mem.resp.fire(), but not stall other ptw req
  val memAddrReg = RegEnable(mem.req.bits.addr, mem.req.fire())
  val l1Hit = Reg(Bool())
  val l2Hit = Reg(Bool())

  val memRdata = mem.resp.bits.data
  val memSelData = memRdata.asTypeOf(Vec(MemBandWidth/XLEN, UInt(XLEN.W)))(memAddrReg(log2Up(l1BusDataWidth/8) - 1, log2Up(XLEN/8)))
  val memPtes = (0 until PtwL3SectorSize).map(i => memRdata((i+1)*XLEN-1, i*XLEN).asTypeOf(new PteBundle))
  val memPte = memSelData.asTypeOf(new PteBundle)
  val memPteReg = RegEnable(memPte, mem.resp.fire())

  // xpn is the vpn or ppn 
  // paddr is the mem request address
  def getBundleAddr(xpn : UInt , paddr : UInt , level : UInt , isVpn : Boolean): UInt = {
    val bias = if(isVpn || (colt_stride == 1) ) {
        paddr((log2Up(l1BusDataWidth/8) - 1), log2Up(XLEN/8))
    }else
    {
        Cat(paddr((log2Up(l1BusDataWidth/8) - 1), log2Up(XLEN/8)) , 0.U(colt_stride_len.W))
    }
    val res = Wire(UInt(xpn.getWidth.W))
    when(level === 0.U)
    {
      //1G
      res := Cat((xpn(xpn.getWidth-1,2 * vpnnLen) - bias)(xpn.getWidth - 2 * vpnnLen - 1 ,0) , 0.U((2 * vpnnLen).W))
    }.elsewhen(level === 1.U)
    {
      //2M
      res := Cat((xpn(xpn.getWidth-1,1 * vpnnLen) - bias)(xpn.getWidth - 1 * vpnnLen - 1 ,0) , 0.U((1 * vpnnLen).W))
    }.otherwise
    {
      //4K
      res := (xpn - bias)
    }

    res 
  }

  val notFound = WireInit(false.B)
  switch (state) {
    is (s_idle) {
      when (io.req.fire()) {
        val req = io.req.bits
        state := s_mem_req
        level := Mux(req.l2Hit, 2.U, Mux(req.l1Hit, 1.U, 0.U))
        ppn := Mux(req.l2Hit || req.l1Hit, io.req.bits.ppn, satp.ppn)
        vpn := io.req.bits.vpn
        l1Hit := req.l1Hit
        l2Hit := req.l2Hit
      }
    }

    is (s_mem_req) {
      when (mem.req.fire()) {
        state := s_mem_resp
      }
    }

    is (s_mem_resp) {
      when (mem.resp.fire()) {
        when (memPte.isLeaf() || memPte.isPf(level)) {
          state := s_resp
          notFound := memPte.isPf(level)
        }.otherwise {
          when (level =/= 2.U) {
            level := levelNext
            state := s_mem_req
          }.otherwise {
            state := s_resp
            notFound := true.B
          }
        }
      }
    }

    is (s_resp) {
      when (io.resp.fire()) {
        state := s_idle
      }
    }
  }

  when (sfence.valid) {
    state := s_idle
    when (state === s_mem_resp && !mem.resp.fire() || state === s_mem_req && mem.req.fire()) {
      sfenceLatch := true.B
    }
  }

  val finish = mem.resp.fire()  && (memPte.isLeaf() || memPte.isPf(level) || level === 2.U)
  val resp = Reg(io.resp.bits.cloneType)

  val hasPageFault = level === 3.U || notFound
  // NOTE : !!!!!! now only support 4KB merge otherwise the function is wrong
  val isMergeable = ColtCanMerge(memPtes,level) && !hasPageFault && level === 2.U
  when (finish && !sfenceLatch) {
    resp.source := RegEnable(io.req.bits.source, io.req.fire())
    resp.resp.pf := hasPageFault
    resp.resp.entry.tag := Mux(isMergeable,getBundleAddr(vpn,memAddrReg,level,true) , vpn)
    resp.resp.entry.ppn := Mux(isMergeable,getBundleAddr(memPte.ppn,memAddrReg,level,false) , memPte.ppn)
    // resp.resp.entry.perm.map(_ := memPte.getPerm())
    // resp.resp.entry.perm.map(_:= memPtes.map(_.getPerm).reduce((a , b) => a & b))
    when(isMergeable)
    {
      resp.resp.entry.perm.map(perm => {
        perm.d := Cat(memPtes.map(_.getPerm.d)).andR
        perm.a := Cat(memPtes.map(_.getPerm.a)).andR
        perm.g := memPte.getPerm.g
        perm.u := memPte.getPerm.u
        perm.x := memPte.getPerm.x
        perm.w := memPte.getPerm.w
        perm.r := memPte.getPerm.r
        })
    }.otherwise
    {
      resp.resp.entry.perm.map(_ := memPte.getPerm())
    }
    resp.resp.entry.level.map(_ := level)
    //TODO： merge logic
    //NOTE: NOW only support 11 and 00 which means that we can either merge 4 ptes or merge no pte 
    resp.resp.len := Mux(isMergeable,3.U,0.U)

  }
  io.resp.valid := state === s_resp
  io.resp.bits := resp
  io.req.ready := state === s_idle

  val l1addr = MakeAddr(satp.ppn, getVpnn(vpn, 2))
  val l2addr = MakeAddr(Mux(l1Hit, ppn, memPteReg.ppn), getVpnn(vpn, 1))
  val l3addr = MakeAddr(Mux(l2Hit, ppn, memPteReg.ppn), getVpnn(vpn, 0))
  mem.req.valid := state === s_mem_req && !sfenceLatch
  mem.req.bits.addr := Mux(level === 0.U, l1addr, Mux(level === 1.U, l2addr, l3addr))

  io.refill.vpn := vpn
  io.refill.level := level
  io.refill.memAddr := memAddrReg
  io.sfenceLatch := sfenceLatch

  XSDebug(p"[fsm] state:${state} level:${level} sfenceLatch:${sfenceLatch} notFound:${notFound}\n")

  XSDebug(p"[fsm] isMergeable:${isMergeable} level:${level} tag_raw:${vpn} bundle_tag:${getBundleAddr(vpn,memAddrReg,level,true)} ppn_raw:${memPte.ppn} bundle_ppn:${getBundleAddr(memPte.ppn,memAddrReg,level,false)}\n")

  // perf
  XSPerfAccumulate("fsm_count", io.req.fire())
  for (i <- 0 until PtwWidth) {
    XSPerfAccumulate(s"fsm_count_source${i}", io.req.fire() && io.req.bits.source === i.U)
  }
  XSPerfAccumulate("fsm_busy", state =/= s_idle)
  XSPerfAccumulate("fsm_idle", state === s_idle)
  XSPerfAccumulate("resp_blocked", io.resp.valid && !io.resp.ready)
  XSPerfAccumulate("mem_count", mem.req.fire())
  XSPerfAccumulate("mem_cycle", BoolStopWatch(mem.req.fire, mem.resp.fire(), true))
  XSPerfAccumulate("mem_blocked", mem.req.valid && !mem.req.ready)

  XSPerfAccumulate("1G_merge_count", io.resp.fire() && resp.resp.len === 3.U && resp.resp.entry.level.getOrElse(0.U).asUInt === 0.U)
  XSPerfAccumulate("2M_merge_count", io.resp.fire() && resp.resp.len === 3.U && resp.resp.entry.level.getOrElse(0.U).asUInt === 1.U)
  XSPerfAccumulate("4K_merge_count", io.resp.fire() && resp.resp.len === 3.U && resp.resp.entry.level.getOrElse(0.U).asUInt === 2.U)
}