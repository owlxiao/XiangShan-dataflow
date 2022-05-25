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

package utils

import chisel3._
import chisel3.experimental.{DataMirror, requireIsHardware}
import chisel3.util._
import chisel3.util.experimental.BoringUtils

import scala.collection.mutable.ListBuffer

class FoundrySRAMExtraIO extends Bundle {
  val sram_type: String = "none"
}

// Foundry-dependent IOs for RFs
class RFExtraIO extends FoundrySRAMExtraIO {
  override val sram_type: String = "rf"

  val trim_fuse_in = Input(UInt(11.W))
  val pwr_mgmt_in = Input(UInt(5.W))
  val sleep_fuse_in = Input(UInt(2.W))
  val pwr_mgmt_out = Output(Bool())
  val ip_reset_b = Input(Bool())
  val wrapper = new Bundle() {
    val rd_clk_en = Input(Bool())
    val wr_clk_en = Input(Bool())
  }
  val fscan = new Bundle() {
    val clkungate = Input(Bool())
    val ram = new Bundle() {
      val bypsel = Input(Bool())
      val wdis_b = Input(Bool())
      val rdis_b = Input(Bool())
      val init_en = Input(Bool())
      val init_val = Input(Bool())
    }
  }
  val output_reset = Input(Bool())
}

// Foundry-dependent IOs for SRAMs
class SRAMExtraIO extends FoundrySRAMExtraIO {
  override val sram_type: String = "sram"

  val trim_fuse_in = Input(UInt(20.W))
  val pwr_mgmt_in = Input(UInt(6.W))
  val sleep_fuse_in = Input(UInt(2.W))
  val pwr_mgmt_out = Output(Bool())
  val ip_reset_b = Input(Bool())
  val wrapper = new Bundle() {
    val clk_en = Input(Bool())
  }
  val fscan = new Bundle() {
    val clkungate = Input(Bool())
    val ram = new Bundle() {
      val bypsel = Input(Bool())
      val wdis_b = Input(Bool())
      val rdis_b = Input(Bool())
      val init_en = Input(Bool())
      val init_val = Input(Bool())
    }
  }
  val output_reset = Input(Bool())
}

object FoundrySRAMExtraIO {
  def apply(singlePort: Boolean): Option[FoundrySRAMExtraIO] = {
    if (singlePort) Some(new SRAMExtraIO) else Some(new RFExtraIO)
  }

  private var sram_id: Int = 0
  val extra_instances: ListBuffer[(Int, FoundrySRAMExtraIO)] = ListBuffer.empty[(Int, FoundrySRAMExtraIO)]
  def addExtraInstance(extra: FoundrySRAMExtraIO): Int = {
    val this_id = sram_id
    // val directions = extra.elements.map(elem => DataMirror.directionOf(elem._2))
    // val data_in = extra.elements.zip(directions).filter(_._2 == ActualDirection.Input).keys
    // val data_out = extra.elements.zip(directions).filter(_._2 == ActualDirection.Output).keys
    for ((name, data) <- extra.elements.toSeq.reverse) {
      if (DataMirror.directionOf(data) == ActualDirection.Input) {
        BoringUtils.addSink(data, s"${name}_$this_id")
      }
      else {
        println(s"source from SRAM: $name, $this_id")
        BoringUtils.addSource(data, s"${name}_$this_id")
      }
    }
    val instance = (this_id, extra)
    extra_instances += instance
    sram_id += 1
    this_id
  }
  def addConnector(extra: Seq[Seq[(Int, FoundrySRAMExtraIO)]]): Unit = {
    for (ex <- extra) {
      for ((index, connector) <- ex) {
        require(extra_instances.exists(_._1 == index))
        val sram = extra_instances.find(_._1 == index).get._2
        for (((name, data), (name1, conn)) <- sram.elements.zip(connector.elements)) {
          require(name == name1)
          if (DataMirror.directionOf(data) == ActualDirection.Input) {
            println(s"source: $name, $index")
            BoringUtils.addSource(conn, s"${name}_$index")
          }
          else {
            println(s"sink: $name, $index")
            BoringUtils.addSink(conn, s"${name}_$index")
          }
        }
      }
    }
    sram_id = 0
    extra_instances.clear()
  }
}

class FoundrySRAMExtraConnector(instances: Seq[(Int, FoundrySRAMExtraIO)]) extends Module {
  val sramTypes = instances.map(_._2.sram_type).distinct
  val sram = sramTypes.map(t => instances.filter(_._2.sram_type == t))
  println("connector", sramTypes)
  println("sram", sram.map(_.length), sram.map(_.map(_._1)))

  // this is from dft controller
  val io = IO(new Bundle() {
    val dummy = Input(Bool())
  })
  // this is from srams
  val extra = sram.map(s => IO(Flipped(Vec(s.length, s.head._2.cloneType))))

  for (ex <- extra) {
    for (elem <- ex) {
      for ((_, data) <- elem.elements.toSeq.reverse) {
        if (DataMirror.directionOf(data) == ActualDirection.Output) {
          data := LFSR64().asTypeOf(data.cloneType)
        }
      }
    }
  }

  dontTouch(io)
  extra.foreach(ex => dontTouch(ex))
}

object FoundrySRAMExtraConnector {
  def apply(): FoundrySRAMExtraConnector = {
    val sram_connector = Module(new FoundrySRAMExtraConnector(FoundrySRAMExtraIO.extra_instances))
    sram_connector.io := DontCare
    sram_connector.extra.foreach(x => x := DontCare)
    val indexes = sram_connector.sram.map(_.map(_._1))
    val extra_io = sram_connector.extra
    FoundrySRAMExtraIO.addConnector(indexes.zip(extra_io).map(x => x._1.zip(x._2)))
    sram_connector
  }
}