package org.shahe.tatu.chip.mmu

import chisel3._
import org.shahe.tatu.chip.common.{TatuModule, TatuBundle}
import chisel3.util.{Decoupled, log2Ceil, Enum}
import chisel3.util.PriorityEncoder
import org.shahe.tatu.chip.csr.PrivStateType
import chisel3.util.MuxLookup

class TLBEntry extends TatuBundle {
    val vpn = UInt(27.W) // Sv39: vpn(2), vpn(1), vpn(0)
    val ppn = UInt(44.W)
    val u = Bool()
    val r = Bool()
    val w = Bool()
    val x = Bool()
    val valid = Bool()
    val level = UInt(2.W)
}

class TLBBundle extends TatuBundle {
    val abort = Input(Bool())
    val satp = Input(UInt(dataWidth.W))
    val priv = Input(UInt(2.W))
    val req = Flipped(Decoupled(new TransUnitReqBundle))
    val resp = Decoupled(new TransUnitRespBundle)
    val sfense = Input(Bool())
}

class TranslationLookupTable(val num_entries: Int = 16) extends TatuModule {
    val io = IO(new TLBBundle)

    // address cache
    val entries = RegInit(VecInit(Seq.fill(num_entries)(0.U.asTypeOf(new TLBEntry))))
    val replacePtr = RegInit(0.U(log2Ceil(num_entries).W))

    // TransUnit (Page Table Walker)
    val ptw = Module(new TransUnit)
    ptw.io.satp := io.satp
    ptw.io.priv := io.priv
    ptw.io.abort := io.abort

    // state machine
    val sIdle :: sWaitPTW :: Nil = Enum(2)
    val state = RegInit(sIdle)

    // full-associative parallel lookup
    val vpnQuery = io.req.bits.va(38, 12)
    val hits = VecInit((0 until num_entries).map(i => {
        val e = entries(i)
        val matchLevel2 = (e.level === 2.U) && (e.vpn(26, 18) === vpnQuery(26, 18))
        val matchLevel1 = (e.level === 1.U) && (e.vpn(26, 9) === vpnQuery(26, 9))
        val matchLevel0 = (e.level === 0.U) && (e.vpn === vpnQuery)
        e.valid && (matchLevel2 || matchLevel1 || matchLevel0)
    }))
    val isHit = hits.asUInt.orR
    val hitIdx = PriorityEncoder(hits)
    val hitEntry = entries(hitIdx)

    // if hit, check privilege sign
    val isPrivU = (io.priv === PrivStateType.user)
    val privDeny = isPrivU && !hitEntry.u
    val accessDeny = MuxLookup(io.req.bits.reqOp, true.B)(Seq(
        MemReqOp.r -> !hitEntry.r,
        MemReqOp.w -> !hitEntry.w,
        MemReqOp.x -> !hitEntry.x
    ))

    // same as TransUnit
    def throwPageFault(op: UInt): UInt = {
        MuxLookup(op, PageFaultExceptType.ipf)(Seq(
            MemReqOp.r -> PageFaultExceptType.lpf,
            MemReqOp.w -> PageFaultExceptType.sapf,
            MemReqOp.x -> PageFaultExceptType.ipf
        ))
    }

    
}
