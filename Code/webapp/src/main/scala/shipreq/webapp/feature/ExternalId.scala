package shipreq.webapp.feature

import net.liftweb.common.{Full, Empty, Box}
import shipreq.base.util.TaggedTypes.TaggedTypeCtor
import shipreq.webapp.util.BaseX
import shipreq.webapp.lib.Types._

object ExternalId {
  // util.Random.shuffle(x.toList).mkString
  final val Project = new ExternalIdConverter[ProjectId]("F4XBvt0i2cnHQ6dIaAomLjPE3MOrsbxReq1W9pgZyzNY7SkGf5UlwJCTKuVD8h")
  final val UseCase = new ExternalIdConverter[UseCaseIdentId]("0atxlQwnj7y3zFZNVBqJ42AcriYEeMu8SdU91HgfTsb6GhmWkX5KopCIRLvOPD")
  final val TextRev = new ExternalIdConverter[TextRevId]("eBM0xKQuO2Zy43AnWGPmkbXN9HprwV7ItSi1CdETv6D5UYRscjJzhFgoLflqa8")
}

final class ExternalIdConverter[I <: ExteralisableId](val dictionaryStr: String)(implicit I: TaggedTypeCtor[I],  E: TaggedTypeCtor[I#E]) {
  type E = I#E

  import ExternalIdFns._

  private[this] val base62 = new BaseX(dictionaryStr, 4)
  require(base62.base.longValue == 62)

  @inline def apply(internal: I): E = toExternal(internal)

  def toExternal(internal: I): E = {
    var (a, b) = splitLong(internal.value)
    b = xorness(b)
    b = shuffleBitsObfuscate(b)
    val x = joinInts(a, b)
    E(base62 encode x)
  }

  private def parse(external: String): I = {
    val y = base62.decode(external)
    var (a, b) = splitLong(y)
    b = shuffleBitsRestore(b)
    b = xorness(b)
    I(joinInts(a, b))
  }

  def isValidExternalId(str: String): Boolean = ExternalIdRegex.matcher(str).matches

  def parseO(external: String): Option[I] = if (isValidExternalId(external)) Some(parse(external)) else None
  def parseB(external: String): Box[I] = if (isValidExternalId(external)) Full(parse(external)) else Empty
  def parseB(str: Box[String]): Box[I] = str.flatMap(parseB)
}

private[feature] object ExternalIdFns {
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
