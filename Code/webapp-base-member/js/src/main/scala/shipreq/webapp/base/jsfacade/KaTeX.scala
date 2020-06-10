package shipreq.webapp.base.jsfacade

import scala.scalajs.js
import scala.scalajs.js.annotation._

/**
 * KaTeX is a fast, easy-to-use JavaScript library for TeX math rendering on the web.
 *
 * https://github.com/Khan/KaTeX
 * https://khan.github.io/KaTeX/
 */
@JSGlobal("katex")
@js.native
object KaTeX extends js.Object {

  //def render(math: String, element: Element): Unit = js.native

  /**
   * @return `"""<span class="katex">...</span>"""`
   * @throws RuntimeException if input is invalid
   */
  @JSName("renderToString")
  def renderToStringUnsafe(tex: String): String = js.native
}
