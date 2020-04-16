package shipreq.webapp.base.data

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

  def rgb: (Int, Int, Int) = {
    def parseHex(hex: String): Int =
      Integer.parseInt(hex, 16)

    val hex = this.hex

    hex.length match {
      case 3 => thrice(hex(0), hex(1), hex(2))(c => parseHex(c.toString + c))
      case 6 => thrice(hex.substring(0, 2), hex.substring(2, 4), hex.substring(4, 6))(parseHex)
    }
  }

  private def thrice[A, B](a: A, b: A, c: A)(f: A => B): (B, B, B) =
    (f(a), f(b), f(c))

  val luminanace: Double = {
    def f(i: Int): Double = {
      val d = i.toDouble / 255
      if (d <= 0.03928)
        d / 12.92
      else
        Math.pow((d + 0.055) / 1.055, 2.4)
    }

    val (r, g, b) = rgb
    f(r) * 0.2126 + f(g) * 0.7152 + f(b) * 0.0722
  }

  def contrastRatio(c: Colour): Double = {
    val lum1      = luminanace
    val lum2      = c.luminanace
    val brightest = Math.max(lum1, lum2)
    val darkest   = Math.min(lum1, lum2)
    (brightest + 0.05) / (darkest + 0.05)
  }

  /** A suggested foreground colour. */
  lazy val foreground: Colour = {
    import Colour.{black, white}

    // Surprisingly, choosing the higher contrast ratio leads to undesirable results.
    // A red (#f00) background has a much higher ratio with black than white, but white is so much easier to read.

    // The following formula was derived and confirmed using widgets/ColourTest.scala
    if (luminanace < .4 && contrastRatio(white) >= 2.35)
      white
    else
      black
  }
}

object Colour {
  // Not allowing users to specify opacity
  private val hexFmt = "^#[0-9a-f]{3}(?:[0-9a-f]{3})?$".r.pattern

  val white = force("#fff")
  val black = force("#000")

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