package shipreq.webapp.base.jsfacade

import japgolly.scalajs.react.vdom.VdomNode
import scalajs.js
import scalajs.js.annotation._

object HtmlReactParser {

  @JSGlobal("HRP")
  @js.native
  private object raw extends js.Object

  def parse(html: String): VdomNode = {
    val out = raw.asInstanceOf[js.Function1[String, js.Object]](html)
    VdomNode.cast(out)
  }

}
