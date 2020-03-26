package shipreq.webapp.base.feature.autocomplete

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html

object InputComponent {
  import Implicits._

  @inline def apply(autoComplete: CallbackTo[Utils.Strategies])(render: TagMod => VdomNode): VdomElement =
    Component(Props(render, autoComplete))

  final case class Props(render: TagMod => VdomNode, autoComplete: CallbackTo[Utils.Strategies])

  final class Backend($: BackendScope[Props, Unit]) extends ForComponent.Backend[html.Input] {

    def render(p: Props): VdomNode =
      p.render(^.onBlur --> autoCompleteBlur)

    override val autoCompleteCtx: CallbackOption[AutoCompleteCtx] =
      for {
        d <- $.getDOMNode.toCBO
        h <- CallbackOption.liftOption(d.toHtml)
        p <- $.props.toCBO
        a <- p.autoComplete.toCBO
      } yield {
        val i = h.querySelector("input").domCast[html.Input]
        AutoCompleteCtx(a, i)
      }
  }

  val Component = ScalaComponent.builder[Props]("InputWithAutoComplete")
    .renderBackend[Backend]
    .configure(ForComponent.install(autoCompletableInput))
    .build
}