package shipreq

// JS
object Predef extends PredefShared {

  @inline def console = org.scalajs.dom.console

  @inline def JSON = scala.scalajs.js.JSON

  @inline def BREAKPOINT() = scala.scalajs.js.special.debugger()

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
