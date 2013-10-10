package com.beardedlogic.usecase.lib

import net.liftweb.common.{Full, Empty, Box}
import com.beardedlogic.usecase.util.BaseX
import Types._

object ExternalId {
  // util.Random.shuffle(x.toList).mkString
  final val Project = new ExternalIdConverter[ProjectIdTag]("F4XBvt0i2cnHQ6dIaAomLjPE3MOrsbxReq1W9pgZyzNY7SkGf5UlwJCTKuVD8h")
  final val UseCase = new ExternalIdConverter[UseCaseIdentIdTag]("0atxlQwnj7y3zFZNVBqJ42AcriYEeMu8SdU91HgfTsb6GhmWkX5KopCIRLvOPD")
  final val TextRev = new ExternalIdConverter[TextRevIdTag]("eBM0xKQuO2Zy43AnWGPmkbXN9HprwV7ItSi1CdETv6D5UYRscjJzhFgoLflqa8")
}

final class ExternalIdConverter[Tag <: ExteralisableIdTag](val dictionaryStr: String) {
  type Id = JLong @@ Tag
  type EI = Tag#EI

  import ExternalIdFns._

  private[this] val base62 = new BaseX(dictionaryStr, 4)
  require(base62.base.longValue == 62)

  @inline final def apply(internal: Id): EI = toExternal(internal)

  def toExternal(internal: Id): EI = {
    var (a, b) = splitLong(internal.longValue)
    b = xorness(b)
    b = shuffleBitsObfuscate(b)
    val x = joinInts(a, b)
    base62.encode(x).tag[Tag#EITag]
  }

  private def parse(external: String): Id = {
    val y = base62.decode(external)
    var (a, b) = splitLong(y)
    b = shuffleBitsRestore(b)
    b = xorness(b)
    joinInts(a, b).tag[Tag]
  }

  def isValidExternalId(str: String): Boolean = ExternalIdRegex.matcher(str).matches

  def parseO(external: String): Option[Id] = if (isValidExternalId(external)) Some(parse(external)) else None

  def parseB(external: String): Box[Id] = if (isValidExternalId(external)) Full(parse(external)) else Empty
  def parseB(str: Box[String]): Box[Id] = str.flatMap(parseB)
}

private[lib] final object ExternalIdFns {
  val ExternalIdRegex = "^[a-zA-Z0-9]{4,11}$".r.pattern

  @inline def splitLong(x: Long): (Int, Int) = ((x >>> 32).toInt, (x.toInt & 0xffffffff))
  @inline def joinInts(a: Int, b: Int): Long = (a.toLong << 32L) | (b & 0xffffffffL)
  @inline def xorness(b: Int) = b ^ ((b & 0x7e7) << 12)

  // -------------------------------------------------------------------------------------------------------------------
  // http://stackoverflow.com/questions/8554286/obfuscating-an-id

  val mask1 = 0x00550055
  val mask2 = 0x0000cccc
  val bits1 = 7
  val bits2 = 14

  @inline def shuffleBitsObfuscate(x: Int): Int = {
    var t = (x ^ (x >> bits1)) & mask1
    val u = x ^ t ^ (t << bits1)
    t = (u ^ (u >> bits2)) & mask2
    u ^ t ^ (t << bits2)
  }
  @inline def shuffleBitsRestore(y: Int): Int = {
    var t = (y ^ (y >> bits2)) & mask2
    val u = y ^ t ^ (t << bits2)
    t = (u ^ (u >> bits1)) & mask1
    u ^ t ^ (t << bits1)
  }
}
