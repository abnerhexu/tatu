package org.shahe.tatu
package chip.common

import chisel3._
import chisel3.util._

case class SysConfig(
    numPageLevels: Int = 3,
    addrWidth: Int = 64,
    fetchWidth: Int = 2,
    instWidth: Int = 32,
    dataWidth: Int = 64
)

trait TatuSysConfig {
    val sConfig: SysConfig

    val numPageLevels = sConfig.numPageLevels
    val addrWidth = sConfig.addrWidth
    val fetchWidth = sConfig.fetchWidth
    val instWidth = sConfig.instWidth
    val dataWidth = sConfig.dataWidth
}

case class CacheConfig(
    l1iCacheByte: Int = 4096,
    l1iCacheNumWays: Int = 4,
    l1iCacheLineByte: Int = 32,
)

trait TatuCacheConfig {
    val cConfig: CacheConfig

    val l1iCacheByte = cConfig.l1iCacheByte
    val l1iCacheNumWays = cConfig.l1iCacheNumWays
    val l1iCacheLineByte = cConfig.l1iCacheLineByte
}