package org.shahe.tatu.chip.csr

import chisel3._
import org.shahe.tatu.chip.common.TatuModule

class csr {
    
}

object PrivStateType {
    val width = 2
    val user = 0.U(width.W)
    val supervisor = 1.U(width.W)
    val machine = 3.U(width.W)
}

class RunningState extends TatuModule {
    val io = IO(new Bundle {
        val privState = Output(UInt(PrivStateType.width.W))
    })
    val privStateReg = RegInit(PrivStateType.machine)
}
