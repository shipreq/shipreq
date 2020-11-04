package shipreq.webapp.member.data

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
  val hex: String =
    value.drop(1)

  val `#rrggbb`: String =
    hex.length match {
      case 3 => "#" + hex(0) + hex(0) + hex(1) + hex(1) + hex(2) + hex(2)
      case 6 => value
    }

  val rgb: (Int, Int, Int) = {
    def parseHex(hex: String): Int =
      Integer.parseInt(hex, 16)

    def thrice[A, B](a: A, b: A, c: A)(f: A => B): (B, B, B) =
      (f(a), f(b), f(c))

    hex.length match {
      case 3 => thrice(hex(0), hex(1), hex(2))(c => parseHex(c.toString + c))
      case 6 => thrice(hex.substring(0, 2), hex.substring(2, 4), hex.substring(4, 6))(parseHex)
    }
  }

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

  def averageContrastRatio(colours: IterableOnce[Colour]): Double = {
    var count = 0
    var sum = 0.0
    for (c <- colours.iterator) {
      count += 1
      sum += contrastRatio(c)
    }
    if (count == 0)
      Double.NaN
    else
      sum / count
  }

  private def greyscaleInt: Int = {
    // https://stackoverflow.com/questions/17615963/standard-rgb-to-grayscale-conversion
    // https://en.wikipedia.org/wiki/Grayscale#Converting_color_to_grayscale
    val cSRGB = if (luminanace <= 0.0031308) 12.92 * luminanace else 1.055 * Math.pow(luminanace, 1 / 2.4) - 0.055
    (cSRGB * 255.0 + 0.5).toInt.min(255)
  }

  def greyscale: Colour = {
    val x = greyscaleInt
    Colour.fromRGB(x, x, x)
  }

  /** A version of this colour to use to indicate it's subject is dead. */
  lazy val dead: Colour = {
    val x = applyOpacity(greyscaleInt, 0.5)
    Colour.fromRGB(x, x, x)
  }

  @inline def live(l: Live): Colour =
    if (l is Live) this else dead

  /** @param x [0,255]
    * @param p [0,1]
    * @return [0,255]
    */
  private def applyOpacity(x: Int, p: Double): Int = {
    assert(x >= 0 && x <= 255)
    assert(p >= 0 && p <= 1)
    (255 - p * (255 - x)).toInt
  }

  def withOpacity(p: Double): Colour = {
    val r = applyOpacity(rgb._1, p)
    val g = applyOpacity(rgb._2, p)
    val b = applyOpacity(rgb._3, p)
    Colour.fromRGB(r, g, b)
  }
}

object Colour {
  // Not allowing users to specify opacity
  private val hexFmt = "^#[0-9a-f]{3}(?:[0-9a-f]{3})?$".r.pattern

  val white = force("#fff")
  val black = force("#000")

  /** This is "main blue" in TagPalette.scala */
  val tagDefault = force("#0076f5")

  def apply(s: String): Option[Colour] = {
    val c = correct(s)
    Option.when(isValid(c))(force(c))
  }

  def fromRGB(r: Int, g: Int, b: Int): Colour =
    force("#%02x%02x%02x".format(r, g, b))

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

  def chooseForegroundOverMultipleBackgroundColours(bgCols: IndexedSeq[Colour]): Colour = {
    val all    = bgCols.length
    val half   = all / 2.0
    val whites = bgCols.count(_.foreground eq Colour.white)
    val blacks = all - whites
    if (whites > half)
      white
    else if (blacks > half)
      black
    else {
      val w = white.averageContrastRatio(bgCols)
      val b = black.averageContrastRatio(bgCols)
      if (w > b)
        white
      else
        black
    }
  }
}