package shipreq.webapp.client.project.app.pages.root

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._
import shipreq.base.util.{Allow, Permission}
import shipreq.webapp.base.data.ExternalPubid
import shipreq.webapp.client.project.app.Style

/** The prompt under the Req Lookup card in which the user can enter a pubid to lookup.
  */
object ReqLookupPrompt {

  sealed abstract class Resolution
  object Resolution {
    case object Blank                    extends Resolution
    case class  Valid(ep: ExternalPubid) extends Resolution
    case object Invalid                  extends Resolution
  }

  final case class Props(edit  : StateSnapshot[String],
                         filter: ExternalPubid => Permission,
                         commit: ExternalPubid => Callback) {

    val resolution: Resolution =
      if (edit.value.isEmpty)
        Resolution.Blank
      else
        ExternalPubid.parse(edit.value).filter(filter(_) is Allow) match {
          case Some(ep) => Resolution.Valid(ep)
          case None     => Resolution.Invalid
        }

    @inline def render = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    def updateText: ReactEventFromInput => Callback =
      _.extract(_.target.value)(t => $.props.flatMap(_.edit setState ExternalPubid.preprocessor(t)))

    def commitOnEnter(commit: Callback): ReactKeyboardEvent => Callback =
      CallbackOption.keyCodeSwitch(_) {
        case KeyCode.Enter => commit
      }

    val base = <.input.text(
      ^.cls       := "prompt",
      ^.size      := 12,
      ^.onChange ==> updateText)

    def render(p: Props): VdomElement = {
      val state: TagMod =
        p.resolution match {
        case Resolution.Blank     => EmptyVdom
        case Resolution.Valid(ep) => ^.onKeyDown ==> commitOnEnter(p commit ep)
        case Resolution.Invalid   => Style.home.reqLookupPromptHasError
      }

      base(^.value := p.edit.value, state)
    }
  }

  val Component = ScalaComponent.builder[Props]("ReqLookupPrompt")
    .renderBackend[Backend]
    .build
}
