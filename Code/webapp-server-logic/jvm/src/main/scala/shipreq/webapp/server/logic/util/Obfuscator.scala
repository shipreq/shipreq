package shipreq.webapp.server.logic.util

import shipreq.base.util.{BaseX, Valid, Validity}
import shipreq.webapp.base.util.Obfuscated

final case class Obfuscator[@specialized(Int, Long) A](
    obfuscate  : A => Obfuscated[A],
    validate   : Obfuscated[A] => Validity,
    deobfuscate: Obfuscated[A] => String \/ A) {

  def xmap[B](f: A => B)(g: B => A): Obfuscator[B] =
    Obfuscator(
      b => obfuscate(g(b)).subst,
      o => validate(o.subst),
      o => deobfuscate(o.subst).map(f))
}

/**
 * Generate a dictionary string using:
 *    scala.util.Random.shuffle("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".toList).mkString
 */
object Obfuscator {

  def long(dictionary: String): Obfuscator[Long] = {
    import Internal._
    type A = Long

    val base62 = new BaseX(dictionary, 4)
    assert(base62.base.longValue == 62)

    val obfuscate: A => Obfuscated[A] =
      internal => {
        val a = longHi(internal)
        var b = longLo(internal)
        b = xorness(b)
        b = shuffleBitsObfuscate(b)
        val x = joinInts(a, b)
        Obfuscated(base62 encode x)
      }

    val validate: Obfuscated[A] => Validity =
      o => Valid.when {
        val n = o.value.length
        n >= 4 && n <= 11 && o.value.forall(c =>
          (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
        )
      }

    val deobfuscate: Obfuscated[A] => String \/ A =
      o =>
        if (validate(o) is Valid) {
          val y = base62.decode(o.value)
          val a = longHi(y)
          var b = longLo(y)
          b = shuffleBitsRestore(b)
          b = xorness(b)
          \/-(joinInts(a, b))
        } else
          -\/(o.value)

    Obfuscator(obfuscate, validate, deobfuscate)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object Internal {
    @inline def longHi(x: Long): Int = (x >>> 32).toInt
    @inline def longLo(x: Long): Int = x.toInt & 0xffffffff
    @inline def xorness(b: Int): Int = b ^ ((b & 0x7e7) << 12)
    @inline def joinInts(a: Int, b: Int): Long = (a.toLong << 32L) | (b & 0xffffffffL)

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
