package shipreq

import org.scalajs.dom.raw.Console
import scala.scalajs.js
import scala.scalajs.js.|

// JS
object Predef extends PredefShared {

  lazy val console: Console =
    (js.Dynamic.global.self || js.Dynamic.global.window).console.asInstanceOf[Console]

  @inline def JSON = scala.scalajs.js.JSON

  @inline def BREAKPOINT() = scala.scalajs.js.special.debugger()

  type JsNumber = Byte | Short | Int | Float | Double

  override implicit def predefExtString(s: String): AnyVal with PredefShared.ExtString =
    new PredefJs.ExtString(s)
}

object PredefJs {
  import java.lang.String

  final class ExtString(private val s: String) extends AnyVal with PredefShared.ExtString {
    override def quote =
      scala.scalajs.js.JSON.stringify(s)
  }

}
