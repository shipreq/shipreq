package shipreq.base.util

import java.math.BigInteger
import BaseX._

object BaseX {
  /** Full range of signed long values + 1 */
  val LongSpectrum = BigInteger.valueOf(Long.MaxValue).subtract(BigInteger.valueOf(Long.MinValue)).add(BigInteger.valueOf(1))
}

/**
 * Originally from http://java.dzone.com/articles/base-x-encoding
 */
final class BaseX(val dictionaryStr: String, minStrLen: Int = 1) {

  private val dictionary = dictionaryStr.toCharArray
  private val charMap = dictionaryStr.toList.zipWithIndex.map(t => (t._1, BigInteger.valueOf(t._2))).toMap
  val base = BigInteger.valueOf(dictionary.length)
  private val offsetForMinStrLen = base.pow(minStrLen - 1)

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
      val digit = charMap(ch)
      result = result.add(digit.multiply(base.pow(len_m_1 - i)))
      i += 1
    }

    result = result.subtract(offsetForMinStrLen)
    if (result.compareTo(LongSpectrum) != -1) result = result.subtract(LongSpectrum)

    result.longValue
  }
}