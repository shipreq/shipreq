package shipreq.webapp.server.logic

import japgolly.microlibs.stdlib_ext.StdlibExt._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.BaseX
import shipreq.webapp.base.data.{ExternalId => XId}

/**
 * Generate a dictionary string using:
 *    scala.util.Random.shuffle("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".toList).mkString
 */
object ExternalId {

  def scheme[T, Id](li: Long => Id, il: Id => Long, dictStr: String): Scheme[T, Id] =
    new Scheme(il, li, dictStr)

  final class Scheme[T, Id](il: Id => Long, li: Long => Id, dictStr: String) {
    import Internal._

    type ExtId = XId[T]

    private[this] val base62 = new BaseX(dictStr, 4)
    require(base62.base.longValue == 62)

    @inline def apply(internal: Id): ExtId =
      toExternal(internal)

    def toExternal(internal: Id): ExtId = {
      var (a, b) = splitLong(il(internal))
      b = xorness(b)
      b = shuffleBitsObfuscate(b)
      val x = joinInts(a, b)
      XId(base62 encode x)
    }

    private def _parse(external: String): Id = {
      val y = base62.decode(external)
      var (a, b) = splitLong(y)
      b = shuffleBitsRestore(b)
      b = xorness(b)
      li(joinInts(a, b))
    }

    def isValidExternalId(str: String): Boolean =
      ExternalIdRegex.matcher(str).matches

    def parseOption(ext: String): Option[Id] =
      Option.when(isValidExternalId(ext))(_parse(ext))

    val parse: String => String \/ Id =
      ext => if (isValidExternalId(ext)) \/-(_parse(ext)) else -\/(ext)
  }

  private object Internal {
    val ExternalIdRegex = "^[a-zA-Z0-9]{4,11}$".r.pattern

    @inline def splitLong(x: Long): (Int, Int) = ((x >>> 32).toInt, x.toInt & 0xffffffff)
    @inline def joinInts(a: Int, b: Int): Long = (a.toLong << 32L) | (b & 0xffffffffL)
    @inline def xorness(b: Int) = b ^ ((b & 0x7e7) << 12)

    // -----------------------------------------------------------------------------------------------------------------
    // http://stackoverflow.com/questions/8554286/obfuscating-an-id

    final val mask1 = 0x00550055
    final val mask2 = 0x0000cccc
    final val bits1 = 7
    final val bits2 = 14

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
}
