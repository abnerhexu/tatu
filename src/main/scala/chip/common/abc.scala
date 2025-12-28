package org.shahe.tatu
package chip.common
import chisel3._
import chisel3.util._

abstract class TatuModule extends Module with TatuSysConfig with TatuCacheConfig {
    override val sConfig: SysConfig = SysConfig()
    override val cConfig: CacheConfig = CacheConfig()
}

abstract class TatuBundle extends Bundle with TatuSysConfig with TatuCacheConfig {
    override val sConfig: SysConfig = SysConfig()
    override val cConfig: CacheConfig = CacheConfig()
}