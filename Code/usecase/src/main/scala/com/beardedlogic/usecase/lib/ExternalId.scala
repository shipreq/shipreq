package com.beardedlogic.usecase.lib

object ExternalId {

  val Base62 = new BaseX("0atxlQwnj7y3zFZNVBqJ42AcriYEeMu8SdU91HgfTsb6GhmWkX5KopCIRLvOPD", 4)
  require(Base62.base.longValue == 62)

  private final val ExternalIdRegex = "^[a-zA-Z0-9]{4,11}$".r.pattern

  @inline private final def splitLong(x: Long): (Int, Int) = ((x >>> 32).toInt, (x.toInt & 0xffffffff))
  @inline private final def joinInts(a: Int, b: Int): Long = (a.toLong << 32L) | (b & 0xffffffffL)
  @inline private final def xorness(b: Int) = b ^ ((b & 0x7e7) << 12)

  def toExternal(internal: Long): String = {
    var (a, b) = splitLong(internal)
    b = xorness(b)
    b = shuffleBitsObfuscate(b)
    val x = joinInts(a, b)
    Base62.encode(x)
  }

  def toInternal(external: String): Long = {
    val y = Base62.decode(external)
    var (a, b) = splitLong(y)
    b = shuffleBitsRestore(b)
    b = xorness(b)
    joinInts(a, b)
  }

  def toInternalOpt(external: String): Option[Long] =
    if (ExternalIdRegex.matcher(external).matches) Some(toInternal(external))
    else None

  // -------------------------------------------------------------------------------------------------------------------
  // http://stackoverflow.com/questions/8554286/obfuscating-an-id

  private final val mask1 = 0x00550055
  private final val mask2 = 0x0000cccc
  private final val bits1 = 7
  private final val bits2 = 14

  @inline private final def shuffleBitsObfuscate(x: Int): Int = {
    var t = (x ^ (x >> bits1)) & mask1
    val u = x ^ t ^ (t << bits1)
    t = (u ^ (u >> bits2)) & mask2
    u ^ t ^ (t << bits2)
  }
  @inline private final def shuffleBitsRestore(y: Int): Int = {
    var t = (y ^ (y >> bits2)) & mask2
    val u = y ^ t ^ (t << bits2)
    t = (u ^ (u >> bits1)) & mask1
    u ^ t ^ (t << bits1)
  }
}