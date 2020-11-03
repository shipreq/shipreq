package shipreq.webapp.base.jsfacade

import org.scalajs.dom.Element
import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSGlobal("Prism")
@js.native
@nowarn
object PrismJs extends js.Any {

  def highlightElement(element : Element,
                       async   : Boolean = js.native,
                       callback: js.Function0[Any] = js.native): Unit = js.native

  @js.native
  object languages extends js.Any {
    @JSBracketAccess
    def add(name: String, value: js.Any): Unit = js.native
  }
}
