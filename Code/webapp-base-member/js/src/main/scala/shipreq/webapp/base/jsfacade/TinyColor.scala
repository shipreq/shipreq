package shipreq.webapp.base.jsfacade

import scala.scalajs.js.annotation._
import scala.scalajs.js.|
import scalajs.js

object TinyColor {

  @JSGlobal("tinycolor")
  @js.native
  private object raw extends js.Object

  def apply(s: String | js.Object): TinyColor =
    raw.asInstanceOf[js.Function1[Any, TinyColor]](s)

  @js.native
  trait Hsl extends js.Object {
    var h: Int
    var s: Double
    var l: Double
    var a: Double
  }

  object Hsl {
    @inline def apply(h: Int, s: Double, l: Double, a: Double = 1): Hsl =
      js.Dynamic.literal(h = h, s = s, l = l, a = a).asInstanceOf[Hsl]
  }

  @js.native
  trait Hsv extends js.Object {
    var h: Int
    var s: Double
    var l: Double
    var a: Double
  }

  @js.native
  trait Rgb extends js.Object {
    var r: Int
    var g: Int
    var b: Int
    var a: Double
  }
}


@js.native
trait TinyColor extends js.Object {
  import TinyColor._

  def isValid(): Boolean

  /** Has leading # */
  @JSName("toHexString")
  def toHex(): String

  def toHsl(): Hsl
  def toHsv(): Hsv
  def toRgb(): Rgb
}
