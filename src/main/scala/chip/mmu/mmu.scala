package org.shahe.tatu
package chip.mmu
import chisel3._
import org.shahe.tatu.chip.common.{TatuBundle, TatuModule}

import chisel3.util._
import peripheral.bus.{AXI4LiteBundle, AXI4LiteMasterModule, AXI4LiteReadData, AXI4LiteWRiteRespType}

class Sv39PTE extends TatuBundle {
    val v = Bool()
    val r = Bool()
    val w = Bool()
    val x = Bool()
    val u = Bool()
    val g = Bool()
    val a = Bool()
    val d = Bool()
    val rsw = UInt(2.W)
    val ppn = UInt(44.W)
    val reserved = UInt(10.W)
}

object MemReqOp {
    val width = 2
    val r = 0.U(width.W)
    val w = 1.U(width.W)
    val x = 2.U(width.W)
}

object PageFaultExceptType {
    val width = 2
    val success = 0.U(width.W)
    val ipf = 1.U(width.W) // instruction pagefault
    val lpf = 2.U(width.W) // load pagefault
    val sapf = 3.U(width.W) // store/amo pagefault
}

class TransUnitReqBundle extends TatuBundle {
    val va = UInt(addrWidth.W)
    val reqOp = UInt(MemReqOp.width.W)
}

class TransUnitRespBundle extends TatuBundle {
    val pa = UInt(addrWidth.W)
}

class TransferBundle extends TatuBundle {
    val addr = Input(UInt(addrWidth.W))
    val readData = Decoupled(new AXI4LiteReadData)
    val start = Input(Bool())
}

class TransUnitBundle extends TatuBundle {
    val req = Flipped(Decoupled(new TransUnitReqBundle))
    val resp = Valid(new TransUnitRespBundle)
    val abort = Input(Bool())
    val satp = Input(UInt(dataWidth.W))
    val exception = Output(UInt(PageFaultExceptType.width.W))
    val xb = new TransferBundle
}

class TransUnitTransfer extends AXI4LiteMasterModule {
    // we already have io
    val xb = IO(new TransferBundle) // Transfer(X) Bundle

    val sIdle :: sReq :: sWait :: Nil = Enum(3)

    val state = RegInit(sIdle)

    io.axi.ar.valid := false.B
    io.axi.ar.bits := DontCare
    io.axi.r.ready := false.B

    xb.readData.valid := false.B
    xb.readData.bits.data := 0.U
    xb.readData.bits.resp := 0.U

    switch(state) {
        is(sIdle) {
            when (xb.start) {
                sendReadAddr(xb.addr)
                state := sReq
            }
        }
        is(sReq) {
            when (io.axi.ar.ready) {
                state := sWait
            }
        }
        is(sWait) {
            io.axi.r.ready := true.B
            when(io.axi.r.valid) {
                xb.readData <> io.axi.r
                state := sIdle
            }
        }
    }
}

class TransUnit extends TatuModule {
    val io = IO(new TransUnitBundle)
    val va = io.req.bits.va
    val reqOp = io.req.bits.reqOp
    val satp = io.satp // satp(63, 60) should be 8.U since we only support Sv39
    // TODO: do not use magic literal numbers
    val isSv39 = (satp(63, 60) === 8.U)
    val basePPN = satp(43, 0)

    val vpns = VecInit(va(20, 12), va(29, 21), va(38, 30))
    val pageOffset = va(11, 0)

    def MkPteAddr(ppn: UInt, index: UInt): UInt = {
        val pa = Cat(ppn, index, 0.U(3.W))
        pa
    }

    val l2Pte = Reg(UInt(addrWidth.W))
    val l1Pte = Reg(UInt(addrWidth.W))
    val l0Pte = Reg(UInt(addrWidth.W))
    val lvPte = Reg(UInt(addrWidth.W)) // last encountered (valid) PTE
    val AXIReadResp = RegInit(0.U(AXI4LiteWRiteRespType.width.W))

    val sIdle :: sL2Req :: sL2Wait :: sL1Req :: sL1Wait :: sL0Req :: sL0Wait :: sDone :: Nil = Enum(8)
    val state = RegInit(sIdle)
    val pa = Reg(UInt(addrWidth.W))
    val translation_valid = RegInit(false.B)
    val translation_excpt = RegInit(PageFaultExceptType.success)

    switch(state) {
        is(sIdle) {
            // AXI4-Lite transfer module
            io.xb.addr := DontCare
            io.xb.start := false.B
            io.exception := PageFaultExceptType.success
            io.resp.valid := false.B
            when(!isSv39) {
                // bare metal, Sv39 not enabled
                pa := va
                translation_valid := true.B
                state := sDone
            }.otherwise {
                when(io.req.valid) {
                    state := sL2Req
                }.otherwise {
                    state := sIdle
                }
            }
        }
        is(sL2Req) {
            val l2PteAddr = MkPteAddr(basePPN, vpns(2))
            io.xb.addr := l2PteAddr
            io.xb.start := true.B
            state := sL2Wait
        }
        is(sL2Wait) {
            when(io.xb.readData.valid) {
                val respData = io.xb.readData.bits.data
                val l2ec = respData.asTypeOf(new Sv39PTE) // l2 entry content
                lvPte := respData // last encountered PTE
                when(!l2ec.v || (io.xb.readData.bits.resp =/= 0.U)) {
                    // throw exception if the entry is not valid,
                    // or readData response status is not zero (ok)
                    translation_valid := false.B
                    state := sDone
                }.elsewhen(l2ec.r || l2ec.w || l2ec.x) {
                    // check if super page, vpn(1), vpn(0) should be 0
                    when(vpns(1) =/= 0.U || vpns(0) =/= 0.U) {
                        translation_valid := false.B
                    }.otherwise {
                        // super page
                        translation_valid := true.B
                        pa := Cat(l2ec.ppn, pageOffset)
                    }
                    state := sDone
                }.otherwise {
                    l2Pte := respData
                    AXIReadResp := io.xb.readData.bits.resp
                    state := sL1Req
                }
            }
            // otherwise, keep in sL2Wait
        }
        is(sL1Req) {
            val l2ec = l2Pte.asTypeOf(new Sv39PTE)
            val l1PteAddr = MkPteAddr(l2ec.ppn, vpns(1))
            io.xb.addr := l1PteAddr
            io.xb.start := true.B
            state := sL1Wait
        }
        is(sL1Wait) {
            when(io.xb.readData.valid) {
                val respData = io.xb.readData.bits.data
                val l1ec = respData.asTypeOf(new Sv39PTE)
                lvPte := respData
                when(!l1ec.v || (io.xb.readData.bits.resp =/= 0.U)) {
                    translation_valid := false.B
                    state := sDone
                }.elsewhen(l1ec.r || l1ec.w || l1ec.x){
                    when(vpns(0) =/= 0.U) {
                        translation_valid := false.B
                    }.otherwise {
                        translation_valid := true.B
                        pa := Cat(l1ec.ppn, pageOffset)
                    }
                }.otherwise {
                    l1Pte := respData
                    AXIReadResp := io.xb.readData.bits.resp
                    state := sL0Req
                }
            }
        }
        is(sL0Req) {
            val l1ec = l1Pte.asTypeOf(new Sv39PTE)
            val l0PteAddr = MkPteAddr(l1ec.ppn, vpns(0))
            io.xb.addr := l0PteAddr
            io.xb.start := true.B
            state := sL0Wait
        }
        is(sL0Wait) {
            when(io.xb.readData.valid) {
                val respData = io.xb.readData.bits.data
                val l0ec = respData.asTypeOf(new Sv39PTE)
                lvPte := respData
                when(!l0ec.v || io.xb.readData.bits.resp =/= 0.U) {
                    io.exception := true.B
                    translation_valid := false.B
                }.otherwise {
                    // addr translation success
                    l0Pte := respData
                    AXIReadResp := io.xb.readData.bits.resp
                    pa := Cat(l0ec.ppn, pageOffset)
                    translation_valid := true.B
                }
                // no matter success of failed, goto sDone
                state := sDone
            }
        }
        is(sDone) {
            // check translation valid
            when(translation_valid) {
                val lvec = lvPte.asTypeOf(new Sv39PTE)
                // check lvPte and if not Sv39 simply do not check any privilege
                io.exception := Mux(isSv39, MuxCase(PageFaultExceptType.success, Array(
                    (!lvec.x && reqOp === MemReqOp.x) -> PageFaultExceptType.ipf,
                    (!lvec.r && reqOp === MemReqOp.r) -> PageFaultExceptType.lpf,
                    (!lvec.w && reqOp === MemReqOp.w) -> PageFaultExceptType.sapf,
                )), PageFaultExceptType.success)
                io.resp.valid := Mux(isSv39, PageFaultExceptType.success, io.exception === PageFaultExceptType.success)
            }.otherwise {
                io.resp.valid := false.B
                // set io.exception if translation invalid
                io.exception := MuxLookup(reqOp, PageFaultExceptType.ipf)(Seq(
                    MemReqOp.r -> PageFaultExceptType.lpf,
                    MemReqOp.w -> PageFaultExceptType.sapf,
                    MemReqOp.x -> PageFaultExceptType.ipf
                ))
            }
            // check privilege info

            io.resp.bits.pa := pa

            state := sIdle
        }
    }

}
