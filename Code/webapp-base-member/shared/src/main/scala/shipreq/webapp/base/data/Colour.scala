package shipreq.webapp.base.data

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.TaggedTypes.TaggedString

/**
  * @param value Hex colour with leading "#"
  */
final class Colour private[Colour](val value: String) extends TaggedString {
  override def toString = s"Colour($value)"

  override def equals(obj: Any): Boolean =
    obj match {
      case c: Colour => value ==* c.value
    }

  /** Either "rgb" or "rrggbb" */
  def hex: String =
    value.drop(1)

//  def rgb: (Int, Int, Int) = {
//    def parseHex(hex: String): Int =
//      Integer.parseInt(hex, 16)
//
//    hex.length match {
//      case 3 => thrice(hex(0), hex(1), hex(2))(c => parseHex(c.toString + c))
//      case 6 => thrice(hex.substring(0, 2), hex.substring(2, 4), hex.substring(4, 6))(parseHex)
//    }
//  }
//
//  private def thrice[A, B](a: A, b: A, c: A)(f: A => B): (B, B, B) =
//    (f(a), f(b), f(c))
}

object Colour {
  // Not allowing users to specify opacity
  private val hexFmt = "^#[0-9a-f]{3}(?:[0-9a-f]{3})?$".r.pattern

  def apply(s: String): Option[Colour] = {
    val c = correct(s)
    Option.when(isValid(c))(force(c))
  }

  def liveCorrect(s: String): String = {
    val a =
      s.toLowerCase
        .filter(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || c == '#')
        .take(20)
    if (a.isEmpty)
      a
    else
      "#" + a.filter(_ != '#')
  }

  def correct(s: String): String =
    liveCorrect(s)

  def isValid(s: String): Boolean =
    hexFmt.matcher(s).matches

  def force(s: String): Colour =
    new Colour(s)

  def parseOption(s: String): String \/ Option[Colour] = {
    val c = correct(s)
    if (c.isEmpty)
      \/-(None)
    else if (isValid(c))
      \/-(Some(force(c)))
    else
      -\/("Not a valid colour. Expected #rgb or #rrggbb")
  }
}