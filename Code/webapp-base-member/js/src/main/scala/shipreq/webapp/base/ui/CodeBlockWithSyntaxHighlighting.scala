package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalajs.js
import shipreq.webapp.base.jsfacade.PrismJs

object CodeBlockWithSyntaxHighlighting {

  @inline def apply(language: Option[String], code: String): VdomElement =
    Props(language, code).render

  private final val txt = "txt"

  private var initialisationPending = true

  private def init(): Unit =
    if (initialisationPending) {
      initialisationPending = false

      // Register "txt" as a format with no highlighting
      PrismJs.languages.add(txt, js.Dynamic.literal())
    }

  final case class Props(language: Option[String], code: String) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  final class Backend($: BackendScope[Props, Unit]) {

    private val ref = Ref[html.Element]

    def render(p: Props): VdomElement =
      <.div( // so that React has a stable root
        <.pre(
          <.code.withRef(ref)(
            ^.cls := s"language-${p.language.getOrElse(txt)}",
            p.code)))

    val highlight: Callback =
      for {
        e <- ref.get
        p <- $.props.toCBO
      } yield {
        init()
        val async = (p.code.length >> 16) != 0 // i.e. len > 65535
        PrismJs.highlightElement(e, async)
      }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount(_.backend.highlight)
    .componentDidUpdate(_.backend.highlight)
    .build
}