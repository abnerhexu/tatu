package org.shahe.tatu
package chip.mmu
import chisel3._
import org.shahe.tatu.chip.common.{TatuBundle, TatuModule}

import chisel3.util._
import peripheral.bus.{AXI4LiteBundle, AXI4LiteMasterModule, AXI4LiteReadData, AXI4LiteWRiteRespType}
import org.shahe.tatu.chip.csr.PrivStateType

class Sv39PTE extends TatuBundle {
    val reserved = UInt(10.W)
    val ppn = UInt(44.W)
    val rsw = UInt(2.W)
    val d = Bool()
    val a = Bool()
    val g = Bool()
    val u = Bool()
    val x = Bool()
    val w = Bool()
    val r = Bool()
    val v = Bool()
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
    val exception = Output(UInt(PageFaultExceptType.width.W))
}

class TransferBundle extends TatuBundle {
    val addr = Flipped(Decoupled(UInt(addrWidth.W)))
    val readData = Decoupled(new AXI4LiteReadData)
}

class TransUnitBundle extends TatuBundle {
    val abort = Input(Bool())
    val satp = Input(UInt(dataWidth.W))
    val priv = Input(UInt(PrivStateType.width.W))
    val req = Flipped(Decoupled(new TransUnitReqBundle))
    val resp = Decoupled(new TransUnitRespBundle)
    val xb = new TransferBundle
}

class TransUnitTransfer extends AXI4LiteMasterModule {
    // we already have io
    val xio = IO(new Bundle {
        val abort = Input(Bool())
        val xb = new TransferBundle // Transfer(X) Bundle
    })
    
    
    val sIdle :: sReq :: sWait :: Nil = Enum(3)

    val state = RegInit(sIdle)
    val addrReg = Reg(UInt(addrWidth.W))

    // default signals
    io.axi.ar.valid := false.B
    io.axi.ar.bits.addr := addrReg
    io.axi.ar.bits.prot := 0.U
    io.axi.r.ready := false.B

    xio.xb.readData.valid := false.B
    xio.xb.readData.bits.data := io.axi.r.bits.data
    xio.xb.readData.bits.resp := io.axi.r.bits.resp
    xio.xb.addr.ready := (state === sIdle)

    switch(state) {
        is(sIdle) {
            when(xio.abort) {
                state := sIdle
            }.otherwise {
                when(xio.xb.addr.valid) {
                    addrReg := xio.xb.addr.bits
                    state := sReq
                }
            }
        }
        is(sReq) {
            // keep the signals for axi4-lite
            when(xio.abort) {
                state := sIdle
            }.otherwise {
                io.axi.ar.valid := true.B
                when(io.axi.ar.fire) {
                    state := sWait
                }
            }
        }
        is(sWait) {
            io.axi.r.ready := true.B
            when(xio.abort) {
                when(io.axi.r.fire) {
                    state := sIdle
                }
            }.otherwise {
                when(io.axi.r.fire) {
                    xio.xb.readData.valid := true.B
                    when (xio.xb.readData.fire) {
                        state := sIdle
                    }
                }
            }
        }
    }
}

class TransUnit extends TatuModule {
    // state machine states
    val sIdle :: sReq :: sWait :: sCheck :: sDone :: Nil = Enum(5)

    val io = IO(new TransUnitBundle)

    // registers
    val state = RegInit(sIdle)
    val reqReg = Reg(new TransUnitReqBundle)
    val satpReg = Reg(UInt(dataWidth.W))
    val pteReg = Reg(new Sv39PTE)
    val level = Reg(UInt(2.W)) // Sv has 2, 1, 0 PTEs
    val exceptionType = RegInit(PageFaultExceptType.success)
    val finalPhyaddr = Reg(UInt(addrWidth.W))
    
    // satp info
    val satpMode = satpReg(63, 60)
    val isSv39 = (satpMode === 8.U)
    val basePPN = satpReg(43, 0)

    // privilege info
    val isPrivU = (io.priv === PrivStateType.user)

    // check if vaddr meets imm extension
    // TODO: remove magic numbers
    val vaHighBits = reqReg.va(63, 39)
    val vaSignBit = reqReg.va(38)
    val vaAddrLegal = (vaHighBits === Fill(25, vaSignBit))

    val vpn = VecInit(reqReg.va(20, 12), reqReg.va(29, 21), reqReg.va(38, 30))
    val pageOffset = reqReg.va(11, 0)

    // default signals
    io.req.ready := (state === sIdle)
    io.resp.valid := (state === sDone)
    io.resp.bits := DontCare
    io.xb.addr.valid := false.B
    io.xb.addr.bits := 0.U
    io.xb.readData.ready := true.B

    def throwPageFault(op: UInt): UInt = {
        MuxLookup(op, PageFaultExceptType.ipf)(Seq(
            MemReqOp.r -> PageFaultExceptType.lpf,
            MemReqOp.w -> PageFaultExceptType.sapf,
            MemReqOp.x -> PageFaultExceptType.ipf
        ))
    }

    switch(state) {
        is(sIdle) {
            when(io.req.fire) {
                reqReg := io.req.bits
                // TODO: do not need to read satp every time
                satpReg := io.satp
                exceptionType := PageFaultExceptType.success
                // If do not enable paged memory (initial),
                // or the privilege is M, then pa = va
                when(satpMode === 0.U || !isPrivU) {
                    finalPhyaddr := io.req.bits.va
                    state := sDone
                }.elsewhen(satpMode === 8.U) {
                    when(!vaAddrLegal) {
                        exceptionType := throwPageFault(io.req.bits.reqOp)
                        state := sDone
                    }.otherwise {
                        level := 2.U
                        state := sReq
                    }
                }.otherwise {
                    exceptionType := throwPageFault(io.req.bits.reqOp)
                    state := sDone
                }
            }
        }
        is(sReq) {
            val currentBase = Mux(level === 2.U, basePPN, pteReg.ppn)
            val pteAddr = Cat(currentBase, vpn(level), 0.U(3.W))

            io.xb.addr.valid := true.B
            io.xb.addr.bits := pteAddr

            when(io.xb.addr.fire) {
                state := sWait
            }
        }

        is(sWait) {
            when(io.xb.readData.valid) {
                val pte = io.xb.readData.bits.data.asTypeOf(new Sv39PTE)
                pteReg := pte
                // check valid and axi error
                when(!pte.v || io.xb.readData.bits.resp =/= 0.U) {
                    exceptionType := throwPageFault(reqReg.reqOp)
                    state := sDone
                }.otherwise {
                    state := sCheck
                }
            }
        }

        is(sCheck) {
            val isLeaf = pteReg.r || pteReg.w || pteReg.x
            when(!isLeaf) {
                when(level === 0.U) {
                    exceptionType := throwPageFault(reqReg.reqOp)
                    state := sDone
                }.otherwise {
                    level := level - 1.U
                    state := sReq
                }
            }.otherwise {
                val privFault = 0.U
                // (a) privilege invalid combination check
                // (a.1) w but not readable
                // (a.2) under U privilege but pte.u is zero
                val invalidPrivComb = (!pteReg.r && pteReg.w) || (isPrivU && !pteReg.u)
                val accessDeny = MuxLookup(reqReg.reqOp, true.B)(Seq(
                    MemReqOp.r -> !pteReg.r,
                    MemReqOp.w -> !pteReg.w,
                    MemReqOp.x -> !pteReg.x
                ))
                // (b) check A/D. If not set, trigger pagefault. Software handles.
                val adFault = !pteReg.a || (reqReg.reqOp === MemReqOp.w && !pteReg.d)
                // (c) align check
                val misAligned = Mux(level === 2.U, pteReg.ppn(17, 0) =/= 0.U, Mux(
                    level === 1.U, pteReg.ppn(8, 0) =/= 0.U, false.B
                ))

                when(invalidPrivComb || accessDeny || adFault || misAligned) {
                    exceptionType := throwPageFault(reqReg.reqOp)
                }.otherwise {
                    val finalPPN = MuxLookup(level, pteReg.ppn)(Seq(
                        2.U -> Cat(pteReg.ppn(43, 18), vpn(1), vpn(0)),
                        1.U -> Cat(pteReg.ppn(43, 9), vpn(0)),
                        0.U -> pteReg.ppn
                    ))
                    finalPhyaddr := Cat(finalPPN, pageOffset)
                    exceptionType := PageFaultExceptType.success
                }
                state := sDone
            }
        }

        is(sDone) {
            io.resp.bits.pa := finalPhyaddr
            io.resp.bits.exception := exceptionType
            when(io.resp.fire) {
                state := sIdle
            }
        }
    }

    when(io.abort) {
        state := sIdle
        exceptionType := PageFaultExceptType.success
        io.req.ready := false.B
        io.resp.valid := false.B
        io.xb.addr.valid := false.B
        io.xb.readData.ready := false.B
    }
}