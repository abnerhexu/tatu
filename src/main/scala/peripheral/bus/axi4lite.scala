package org.shahe.tatu
package peripheral.bus

import chisel3._
import chisel3.util._
import org.shahe.tatu.chip.common.{TatuBundle, TatuModule}

class AXI4LiteAddress extends TatuBundle {
    val addr = UInt(addrWidth.W)
    val prot = UInt(3.W)
}

class AXI4LiteWriteData extends TatuBundle {
    val data = UInt(dataWidth.W)
    val strb = UInt((dataWidth / 8).W)
}

object AXI4LiteWRiteRespType {
    val width = 2
    val ok = 0.U(width.W)
    val exok = 1.U(width.W) // not used in AXI4-Lite; reserved for AXI4; exclusively ok
    val slverr = 2.U(width.W) // slave error
    val decerr = 3.U(width.W) // decode error; address not valid or allocated
}

class AXI4LiteWriteResp extends TatuBundle {
    val resp = UInt(AXI4LiteWRiteRespType.width.W)
}

class AXI4LiteReadData extends TatuBundle {
    val data = UInt(dataWidth.W)
    val resp = UInt(AXI4LiteWRiteRespType.width.W)
}

class AXI4LiteBundle extends TatuBundle {
    val aw = Decoupled(new AXI4LiteAddress) // port for write address
    val w = Decoupled(new AXI4LiteWriteData) // port for write data
    val b = Flipped(Decoupled(new AXI4LiteWriteResp)) // port for write response
    val ar = Decoupled(new AXI4LiteAddress) // port for read address
    val r = Flipped(Decoupled(new AXI4LiteReadData)) // port for read data
}

abstract class AXI4LiteMasterModule extends TatuModule {
    val io = IO(new Bundle {
        val axi = new AXI4LiteBundle
    })

    def sendReadAddr(addr: UInt): Unit = {
        io.axi.ar.valid := true.B
        io.axi.ar.bits.addr := addr
        io.axi.ar.bits.prot := 3.U // or 0.U
    }

    def sendWrite(addr: UInt, data: UInt, strb: UInt = "b11111111".U): Unit = {
        // TODO: avoid magic literal number b11111111
        io.axi.aw.valid := true.B
        io.axi.aw.bits.addr := addr
        io.axi.w.valid := true.B
        io.axi.w.bits.data := data
        io.axi.w.bits.strb := strb
    }
}