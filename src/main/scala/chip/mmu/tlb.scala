package org.shahe.tatu.chip.mmu

import chisel3._
import org.shahe.tatu.chip.common.{TatuModule, TatuBundle}
import chisel3.util._
import org.shahe.tatu.chip.csr.PrivStateType

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

    io.req.ready := false.B
    io.resp.valid := false.B
    io.resp.bits := DontCare

    ptw.io.req.valid := false.B
    ptw.io.req.bits := io.req.bits
    ptw.io.resp.ready := false.B

    val mmuDisabled = (io.satp(63, 60) === 0.U) || (io.priv === PrivStateType.machine)

    switch(state) {
        is(sIdle) {
            when(io.req.valid) {
                when(mmuDisabled) {
                    // case (a): mmu not enabled, direct pa
                    io.resp.valid := true.B
                    io.resp.bits.pa := io.req.bits.va
                    io.resp.bits.exception := PageFaultExceptType.success
                    io.req.ready := io.resp.ready
                }.elsewhen(isHit) {
                    // case (b): tlb hit
                    io.resp.valid := true.B
                    io.req.ready := true.B

                    io.resp.bits.pa := MuxLookup(hitEntry.level, Cat(hitEntry.ppn, io.req.bits.va(11, 0)))(Seq(
                        2.U -> Cat(hitEntry.ppn(43, 18), io.req.bits.va(29, 0)),
                        1.U -> Cat(hitEntry.ppn(43, 9), io.req.bits.va(20, 0)),
                        0.U -> Cat(hitEntry.ppn, io.req.bits.va(11, 0))
                    ))

                    when(privDeny || accessDeny) {
                        io.resp.bits.exception := throwPageFault(io.req.bits.reqOp)
                    }.otherwise {
                        io.resp.bits.exception := PageFaultExceptType.success
                    }
                }.otherwise {
                    // case (c): tlb miss, go to ptw
                    ptw.io.req.valid := true.B
                    when(ptw.io.req.fire) {
                        state := sWaitPTW
                    }
                }
            }
        }

        is(sWaitPTW) {
            ptw.io.resp.ready := io.resp.ready
            io.resp.valid := ptw.io.resp.valid
            io.resp.bits := ptw.io.resp.bits

            when(ptw.io.resp.fire) {
                // only update TLB when PTW finds pagetable and no fault occurs
                when(ptw.io.resp.bits.exception === PageFaultExceptType.success) {
                    val replaced = entries(replacePtr)
                    replaced.valid := true.B
                    replaced.vpn := vpnQuery
                    replaced.ppn := ptw.io.resp.bits.pte.ppn
                    replaced.u := ptw.io.resp.bits.pte.u
                    replaced.r := ptw.io.resp.bits.pte.r
                    replaced.w := ptw.io.resp.bits.pte.w
                    replaced.x := ptw.io.resp.bits.pte.x
                    replaced.level := ptw.io.resp.bits.level

                    replacePtr := replacePtr + 1.U
                }
                state := sIdle
            }
        }
    }

    when(io.sfense) {
        entries.foreach(_.valid := false.B)
    }
}
