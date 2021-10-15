package shipreq.webapp.member.feature.autocomplete

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import shipreq.webapp.member.feature.AutoCompleteFeature
import shipreq.webapp.member.feature.autocomplete.strategies.Strategies

object InputComponent {
  import Implicits._

  @inline def apply(autoComplete: CallbackTo[Strategies])(render: TagMod => VdomNode): VdomElement =
    Component(Props(render, autoComplete))

  final case class Props(render: TagMod => VdomNode, autoComplete: CallbackTo[Strategies])

  final class Backend($: BackendScope[Props, Unit]) extends ForComponent.Backend[html.Input] {

    def render(p: Props): VdomNode =
      p.render(TagMod(
        ^.onBlur --> autoCompleteOnBlur,
        ^.onClick ==> autoCompleteOnClick,
        ^.onKeyDown ==> autoCompleteOnKeyDown,
      ))

    override val autoCompleteCtx: CallbackOption[AutoCompleteCtx] =
      for {
        d <- $.getDOMNode.toCBO
        h <- CallbackOption.option(d.toHtml)
        p <- $.props.toCBO
        a <- p.autoComplete.toCBO
      } yield {
        val i = h.querySelector("input").domCast[html.Input]
        AutoCompleteCtx(a, i)
      }

    override protected def getTextFromHeadToCaret =
      AutoCompleteFeature.AutoComplete.getTextFromHeadToCaretI
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(ForComponent.install(autoCompletableInput))
    .build
}