package shipreq.webapp.client.util

import org.scalajs.dom.Element
import scalajs.js.annotation.JSName
import scalajs.js._

/**
 * KaTeX is a fast, easy-to-use JavaScript library for TeX math rendering on the web.
 *
 * https://github.com/Khan/KaTeX
 * http://khan.github.io/KaTeX/
 */
@JSName("katex")
object KaTeX extends Object {

  //def render(math: String, element: Element): Unit = native

  /**
   * @return `"""<span class="katex">...</span>"""`
   * @throws RuntimeException if input is invalid
   */
  @JSName("renderToString")
  def renderToStringUnsafe(math: String): String = native
}
