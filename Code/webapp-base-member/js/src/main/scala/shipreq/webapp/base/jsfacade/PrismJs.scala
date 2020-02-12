package shipreq.webapp.base.jsfacade

import org.scalajs.dom.Element
import scala.scalajs.js.annotation._
import scalajs.js

@JSGlobal("Prism")
@js.native
object PrismJs extends js.Any {

  def highlightElement(element : Element,
                       async   : Boolean = js.native,
                       callback: js.Function0[Any] = js.native): Unit = js.native

}
