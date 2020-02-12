package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import shipreq.webapp.base.jsfacade.PrismJs

object CodeBlockWithSyntaxHighlighting {

  final case class Props(language: String, code: String) {
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
            ^.cls := s"language-${p.language}",
            p.code)))

    val highlight: Callback =
      for {
        e <- ref.get
        p <- $.props.toCBO
      } yield {
        val async = (p.code.length >> 16) != 0 // i.e. len > 65535
        PrismJs.highlightElement(e, async)
      }
  }

  val Component = ScalaComponent.builder[Props]("CodeBlockWithSyntaxHighlighting")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .componentDidMount(_.backend.highlight)
    .componentDidUpdate(_.backend.highlight)
    .build
}