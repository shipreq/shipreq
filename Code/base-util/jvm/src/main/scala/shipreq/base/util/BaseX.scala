package shipreq.base.util

import java.math.BigInteger
import scala.reflect.ClassTag
import shipreq.base.util.BaseX._

object BaseX {
  /** Full range of signed long values + 1 */
  val LongSpectrum = BigInteger.valueOf(Long.MaxValue).subtract(BigInteger.valueOf(Long.MinValue)).add(BigInteger.valueOf(1))
}

/**
 * Originally from http://java.dzone.com/articles/base-x-encoding
 */
final class BaseX(val dictionaryStr: String, minStrLen: Int = 1) {

  private val dictionary: Array[Char] = dictionaryStr.toCharArray

  private val dictMin: Char = dictionary.min
  private val dictMax: Char = dictionary.max

  private def cacheByChar[A: ClassTag](f: (Char, Int) => A): Char => A = {
    val m = dictionaryStr.toList.zipWithIndex.map(t => (t._1, f(t._1, t._2))).toMap
    val a = new Array[A](dictMax - dictMin + 1)
    for ((c, b) <- m)
      a(c - dictMin) = b
    c => a(c - dictMin)
  }

  private def cacheByPower[A: ClassTag](f: Int => A): Int => A = {
    val n = 11 // can't remember why but in Obfuscator.scala encoded strings are expected to always be [4,11] chars
    val a = new Array[A](n)
    for (i <- a.indices)
      a(i) = f(i)
    a.apply
  }

  // Cache 1: +100,000 ops/s
  private val charMap: Char => BigInteger =
    cacheByChar((_, i) => BigInteger.valueOf(i))

  val base = BigInteger.valueOf(dictionary.length)

  // Cache 2: +1,000,000 ops/s
  private val powerCache: Int => BigInteger =
    cacheByPower(base.pow)

  // Cache 3: +1,600,000 ops/s
  private val decodeMap: (Char, Int) => BigInteger = {
    val a = cacheByPower(i => cacheByChar((c, _) => charMap(c).multiply(powerCache(i))))
    (c, i) => a(i)(c)
  }

  private val offsetForMinStrLen = powerCache(minStrLen - 1)

  def encode(longValue: Long): String = {
    var value = BigInteger.valueOf(longValue)
    if (value.signum == -1)
      value = value.add(LongSpectrum)
    value = value.add(offsetForMinStrLen)

    val s = new java.lang.StringBuilder
    var parts = new Array[BigInteger](2)
    parts(0) = value
    parts(1) = BigInteger.ZERO

    while (parts(0).compareTo(base) >= 0) {
      parts = parts(0).divideAndRemainder(base)
      s.append(dictionary(parts(1).intValue))
    }
    s.append(dictionary(parts(0).intValue))
    s.reverse.toString
  }

  def decode(encoded: String): Long = {
    var result = BigInteger.ZERO
    val len_m_1 = encoded.length - 1
    var i = 0
    for (ch <- encoded) {
      result = result add decodeMap(ch, len_m_1 - i)
      i += 1
    }
    result = result.subtract(offsetForMinStrLen)
    if (result.compareTo(LongSpectrum) != -1) result = result.subtract(LongSpectrum)
    result.longValue
  }
}