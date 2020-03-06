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
}

object Colour {
  // Not allowing users to specify opacity
  private val hexFmt = "^#[0-9a-f]{3}(?:[0-9a-f]{3})?$".r.pattern

  def apply(s: String): Option[Colour] = {
    val c = correct(s)
    Option.when(isValid(c))(force(c))
  }

  def liveCorrect(s: String): String =
    s.trim.take(20).toLowerCase

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