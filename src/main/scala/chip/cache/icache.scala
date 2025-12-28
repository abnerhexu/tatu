package org.shahe.tatu
package chip.cache
import chisel3._
import chip.common.{TatuBundle, TatuCacheConfig, TatuModule}

import chisel3.util.{Decoupled, Valid, log2Ceil}

class InstCacheReqBundle extends TatuBundle {
    val va = UInt(addrWidth.W)
}

class InstCacheRespBundle extends TatuBundle {
    val inst = UInt((fetchWidth*instWidth).W)
    val replay = Bool()
}

class InstCacheBundleS0 extends TatuBundle {
    val req = Flipped(Decoupled(new InstCacheReqBundle))
    val fwd = Valid(???)
    val flush = Input(Bool())
}

class InstCacheS0 extends TatuModule {
    // stage 0: extract the va and corresponding set index
    val io = IO(new InstCacheBundleS0)
    val valid = io.req.valid
    val va = io.req.bits.va

    val numSet = l1iCacheByte / l1iCacheNumWays / l1iCacheLineByte
    val setIdx = va(log2Ceil(numSet)+log2Ceil(l1iCacheByte)-1, log2Ceil(l1iCacheByte)+0)


}
