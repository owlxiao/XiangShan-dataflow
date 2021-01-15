package xiangshan.frontend

import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.ALUOpType
import utils._
import chisel3.experimental.chiselName

class RASEntry() extends XSBundle {
    val retAddr = UInt(VAddrBits.W)
    val ctr = UInt(8.W) // layer of nested call functions
}

@chiselName
class RAS extends BasePredictor
{
    class RASResp extends Resp
    {
        val target =UInt(VAddrBits.W)
    }

    class RASBranchInfo extends Meta
    {
        val rasSp = UInt(log2Up(RasSize).W)
        val rasTop = new RASEntry
    }

    class RASIO extends DefaultBasePredictorIO 
    {
        val is_ret = Input(Bool())
        val callIdx = Flipped(ValidIO(UInt(log2Ceil(PredictWidth).W)))
        val isRVC = Input(Bool())
        val isLastHalfRVI = Input(Bool())
        val redirect =  Flipped(ValidIO(new Redirect))
        val out = ValidIO(new RASResp)
        val meta = Output(new RASBranchInfo)
    }

    
    def rasEntry() = new RASEntry

    object RASEntry {
        def apply(retAddr: UInt, ctr: UInt): RASEntry = {
            val e = Wire(rasEntry())
            e.retAddr := retAddr
            e.ctr := ctr
            e
        }
    }

    override val io = IO(new RASIO)
    override val debug = true

    @chiselName
    class RASStack(val rasSize: Int) extends XSModule {
        val io = IO(new Bundle {
            val push_valid = Input(Bool())
            val pop_valid = Input(Bool())
            val new_addr = Input(UInt(VAddrBits.W))
            val top_addr = Output(UInt(VAddrBits.W))
            val is_empty = Output(Bool())
            val is_full = Output(Bool())
            val copy_valid = Input(Bool())
            val copy_in_mem  = Input(Vec(rasSize, rasEntry()))
            val copy_in_sp   = Input(UInt(log2Up(rasSize).W))
            val copy_in_top  = Input(rasEntry())
            val copy_out_mem = Output(Vec(rasSize, rasEntry()))
            val copy_out_sp  = Output(UInt(log2Up(rasSize).W))
            val copy_out_top  = Output(rasEntry())

        })
        val debugIO = IO(new Bundle{
            val write_entry = Output(rasEntry())
            val alloc_new = Output(Bool())
            val sp = Output(UInt(log2Up(rasSize).W))
            val topRegister = Output(rasEntry())
        })
        @chiselName
        class Stack(val size: Int) extends XSModule {
            val io = IO(new Bundle {
                val rIdx = Input(UInt(log2Up(size).W))
                val rdata = Output(rasEntry())
                val wen = Input(Bool())
                val wIdx = Input(UInt(log2Up(size).W))
                val wdata = Input(rasEntry())
                val copyen = Input(Bool())
                val copy_in = Input(Vec(size, rasEntry()))
                val copy_out = Output(Vec(size, rasEntry()))
            })
            val mem = Reg(Vec(size, rasEntry()))
            when (io.wen)  {
                mem(io.wIdx) := io.wdata
            }
            io.rdata := mem(io.rIdx)
            (0 until size).foreach { i => io.copy_out(i) := mem(i) }
            when (io.copyen) {
                (0 until size).foreach {i => mem(i) := io.copy_in(i) }
            }
        }
        val sp = RegInit(RasSize.U((log2Up(rasSize) + 1).W))
        val topRegister = RegInit(0.U.asTypeOf(new RASEntry))
        val stack = Module(new Stack(rasSize)).io

        stack.rIdx := sp - 1.U
        val top_addr = topRegister.retAddr
        val top_ctr = topRegister.ctr
        val alloc_new = io.new_addr =/= top_addr
        // stack.wen := io.push_valid || io.pop_valid && top_ctr =/= 1.U
        // stack.wIdx := Mux(io.pop_valid && top_ctr =/= 1.U, sp - 1.U, Mux(alloc_new, sp, sp - 1.U))
        // val write_addr = Mux(io.pop_valid && top_ctr =/= 1.U, top_addr, io.new_addr)
        // val write_ctr  = Mux(io.pop_valid && top_ctr =/= 1.U, top_ctr - 1.U, Mux(alloc_new, 1.U, top_ctr + 1.U))

        stack.wen := io.push_valid && !io.is_empty
        stack.wIdx := sp
        val write_addr = topRegister.retAddr
        val write_ctr  = topRegister.ctr

        val write_entry = RASEntry(write_addr, write_ctr)
        stack.wdata := write_entry
        debugIO.write_entry := write_entry
        debugIO.alloc_new := alloc_new
        debugIO.sp := sp
        debugIO.topRegister := topRegister

        val is_empty = sp === RasSize.U
        val is_full  = sp === (RasSize - 1).U
        
        when (io.push_valid && alloc_new) {
            sp := Mux(is_full, sp, Mux(is_empty, 0.U,sp + 1.U))
            top_addr := io.new_addr
            top_ctr := 1.U
        } .elsewhen(io.push_valid) {
            top_ctr := top_ctr + 1.U
        }

        when (io.pop_valid && top_ctr === 1.U) {
            sp := Mux(is_empty, sp ,Mux(sp === 0.U, RasSize.U,sp - 1.U))
            top_addr := stack.rdata.retAddr
            top_ctr := stack.rdata.ctr
        } .elsewhen(io.pop_valid) {
            top_ctr := top_ctr - 1.U
        }

        io.copy_out_mem := stack.copy_out
        io.copy_out_sp  := sp
        io.copy_out_top  := topRegister
        if(EnableCommit){
            stack.copyen := io.copy_valid
            stack.copy_in := io.copy_in_mem
        } else {
            stack.copyen := false.B
            stack.copy_in := DontCare
        }
        when (io.copy_valid) {
            sp := io.copy_in_sp
            topRegister := io.copy_in_top
        }

        io.top_addr := top_addr
        io.is_empty := is_empty
        io.is_full  := is_full
    }

    // val ras_0 = Reg(Vec(RasSize, rasEntry()))  //RegInit(0.U)asTypeOf(Vec(RasSize,rasEntry)) cause comb loop
    // val ras_1 = Reg(Vec(RasSize, rasEntry()))
    // val sp_0 = RegInit(0.U(log2Up(RasSize).W))
    // val sp_1 = RegInit(0.U(log2Up(RasSize).W))
    // val choose_bit = RegInit(false.B)   //start with 0
    // val spec_ras = Mux(choose_bit, ras_1, ras_0)
    // val spec_sp = Mux(choose_bit,sp_1,sp_0)
    // val commit_ras = Mux(choose_bit, ras_0, ras_1)
    // val commit_sp = Mux(choose_bit,sp_0,sp_1)

    // val spec_ras = Reg(Vec(RasSize, rasEntry()))
    // val spec_sp = RegInit(0.U(log2Up(RasSize).W))
    // val commit_ras = Reg(Vec(RasSize, rasEntry()))
    // val commit_sp = RegInit(0.U(log2Up(RasSize).W))

    val spec = Module(new RASStack(RasSize))
    val spec_ras = spec.io


    val spec_push = WireInit(false.B)
    val spec_pop = WireInit(false.B)
    val jump_is_first = io.callIdx.bits === 0.U
    val call_is_last_half = io.isLastHalfRVI && jump_is_first
    val spec_new_addr = packetAligned(io.pc.bits) + (io.callIdx.bits << instOffsetBits.U) + Mux( (io.isRVC | call_is_last_half) && HasCExtension.B, 2.U, 4.U)
    spec_ras.push_valid := spec_push
    spec_ras.pop_valid  := spec_pop
    spec_ras.new_addr   := spec_new_addr
    val spec_is_empty = spec_ras.is_empty
    val spec_is_full = spec_ras.is_full
    val spec_top_addr = spec_ras.top_addr

    spec_push := !spec_is_full && io.callIdx.valid && io.pc.valid
    spec_pop  := !spec_is_empty && io.is_ret && io.pc.valid

    val copy_valid = io.redirect.valid
    val copy_next = RegNext(copy_valid)
    // val copy_bits = RegNext(io.recover.bpuMeta)

    if(EnableCommit){
        val commit_cfi = io.redirect.bits.cfiUpdate
        val commit = Module(new RASStack(RasSize))
        val commit_ras = commit.io

        val commit_push = WireInit(false.B)
        val commit_pop = WireInit(false.B)
        val commit_new_addr = Mux(commit_cfi.pd.isRVC && HasCExtension.B, commit_cfi.pc + 2.U, commit_cfi.pc + 4.U)
        commit_ras.push_valid := commit_push
        commit_ras.pop_valid  := commit_pop
        commit_ras.new_addr   := commit_new_addr
        val commit_is_empty = commit_ras.is_empty
        val commit_is_full = commit_ras.is_full
        val commit_top_addr = commit_ras.top_addr

        val update_valid = io.update.valid
        val update = io.update.bits
        val update_call_valid = update_valid && update.cfiIsCall && update.cfiIndex.valid && update.valids(update.cfiIndex.bits)
        val update_ret_valid  = update_valid && update.cfiIsRet && update.cfiIndex.valid && update.valids(update.cfiIndex.bits)
        commit_push := !commit_is_full  && update_call_valid
        commit_pop  := !commit_is_empty && update_ret_valid

        commit_ras.copy_valid := false.B
        commit_ras.copy_in_mem := DontCare
        commit_ras.copy_in_sp  := DontCare
        commit_ras.copy_in_top := DontCare

        spec_ras.copy_valid := copy_next
        spec_ras.copy_in_mem := commit_ras.copy_out_mem

        spec_ras.copy_in_sp  := commit_ras.copy_out_sp
        spec_ras.copy_in_top := commit_ras.copy_out_top

        //no need to pass the ras branchInfo
        io.meta.rasSp := DontCare
        io.meta.rasTop := DontCare

        if (BPUDebug && debug) {
            val commit_debug = commit.debugIO
            XSDebug("----------------RAS(commit)----------------\n")
            XSDebug(" TopRegister: 0x%x   %d \n",commit_debug.topRegister.retAddr,commit_debug.topRegister.ctr)
            XSDebug("  index       addr           ctr \n")
            for(i <- 0 until RasSize){
                XSDebug("  (%d)   0x%x      %d",i.U,commit_ras.copy_out_mem(i).retAddr,commit_ras.copy_out_mem(i).ctr)
                when(i.U === commit_ras.copy_out_sp){XSDebug(false,true.B,"   <----sp")}
                XSDebug(false,true.B,"\n")
            }
            XSDebug(commit_push, "(commit_ras)push  inAddr: 0x%x  inCtr: %d |  allocNewEntry:%d |   sp:%d  | TopReg.addr %x ctr:%d\n",commit_new_addr,commit_debug.write_entry.ctr,commit_debug.alloc_new,commit_debug.sp.asUInt,commit_debug.topRegister.retAddr,commit_debug.topRegister.ctr)
            XSDebug(commit_pop, "(commit_ras)pop outValid:%d  outAddr: 0x%x \n",io.out.valid,io.out.bits.target)
        }

    } else {
        val retMissPred = copy_valid && io.redirect.bits.level === 0.U && io.redirect.bits.cfiUpdate.pd.isRet
        val recoverSp = io.redirect.bits.cfiUpdate.rasSp
        val recoverTopAddr = io.redirect.bits.cfiUpdate.rasEntry.retAddr
        val recoverTopCtr = io.redirect.bits.cfiUpdate.rasEntry.ctr
        spec_ras.copy_valid := copy_valid
        spec_ras.copy_in_mem := DontCare

        spec_ras.copy_in_sp  := Mux(retMissPred && recoverTopCtr === 1.U ,recoverSp - 1.U,recoverSp)
        spec_ras.copy_in_top.retAddr := recoverTopAddr
        spec_ras.copy_in_top.ctr     := Mux(!retMissPred , recoverTopCtr, Mux(recoverTopCtr === 1.U,recoverTopCtr,  recoverTopCtr - 1.U))

        io.meta.rasSp := spec_ras.copy_out_sp
        io.meta.rasTop := spec.debugIO.topRegister

    }

    io.out.valid := !spec_is_empty
    io.out.bits.target := spec_top_addr
    // TODO: back-up stack for ras
    // use checkpoint to recover RAS

    if (BPUDebug && debug) {
        val spec_debug = spec.debugIO
        XSDebug("----------------RAS(spec)----------------\n")
        XSDebug(" TopRegister: 0x%x   %d \n",spec_debug.topRegister.retAddr,spec_debug.topRegister.ctr)
        XSDebug("  index       addr           ctr \n")
        for(i <- 0 until RasSize){
            XSDebug("  (%d)   0x%x      %d",i.U,spec_ras.copy_out_mem(i).retAddr,spec_ras.copy_out_mem(i).ctr)
            when(i.U === spec_ras.copy_out_sp){XSDebug(false,true.B,"   <----sp")}
            XSDebug(false,true.B,"\n")
        }
        XSDebug(spec_push, "(spec_ras)push  inAddr: 0x%x  inCtr: %d |  allocNewEntry:%d |   sp:%d \n",spec_new_addr,spec_debug.write_entry.ctr,spec_debug.alloc_new,spec_debug.sp.asUInt)
        XSDebug(spec_pop, "(spec_ras)pop outValid:%d  outAddr: 0x%x \n",io.out.valid,io.out.bits.target)

        XSDebug("copyValid:%d copyNext:%d recover(SP:%d retAddr:%x ctr:%d) \n",copy_valid,copy_next,io.redirect.bits.cfiUpdate.rasSp,io.redirect.bits.cfiUpdate.rasEntry.retAddr,io.redirect.bits.cfiUpdate.rasEntry.ctr)
    }

}
