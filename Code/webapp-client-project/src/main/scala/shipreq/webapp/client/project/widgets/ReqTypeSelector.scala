package shipreq.webapp.client.project.widgets

import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.ui.EditTheme
import shipreq.webapp.client.base.ui.semantic._
import shipreq.webapp.client.project.app.Style.widgets.{reqTypeSelector => *}

object ReqTypeSelector {

  type RT = CustomReqType
  type AbortCommit = shipreq.webapp.client.base.lib.AbortCommit[Callback, RT ~=> Callback]

  final case class Props(initialValue: RT,
                         edit        : StateSnapshot[RT],
                         choices     : NonEmptySet[RT],
                         asyncStatus : Option[EditorStatus.Async],
                         abortCommit : AbortCommit) {

    val changed = edit.value !=* initialValue
    def abort = abortCommit.abort
    def commit = if (changed) abortCommit.commit(edit.value) else Callback.empty
    val status = asyncStatus.getOrElse(if (changed) EditorStatus.Valid(commit) else EditorStatus.Ignore)

    @inline def render: VdomElement = Component(this)
  }

  // implicit val reusabilityProps: Reusability[Props] =
  //   Reusability.caseClass

  private def key(rt: RT): Select.OptionKey =
    rt.id.value.toString

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): VdomElement =
      EditTheme.renderEditor(
        status       = p.status,
        editor       = _ => editor(p),
        readOnlyView = p.edit.value.fullName,
        instructions = EmptyVdom)

    def editor(p: Props): VdomElement = {
      val options =
        MutableArray(p.choices.whole)
          .map(rt => Select.Option(key(rt), rt.fullName, rt))
          .sort
          .to[List]

      val select = Select(options, key(p.edit.value))(p.edit setState _.value)(*.dropdown)

      val commitButton = Button(
        tipe = Button.Type.IconOnly(Icon.Checkmark),
        state = Button.State.enabledWhen(p.changed)
      ).tag(
        *.commit,
        ^.onClick --> p.commit)

      val abortButton = Button(tipe = Button.Type.IconOnly(Icon.Remove)).tag(
        *.abort,
        ^.onClick --> p.abort)

      val buttons = Button.group(commitButton, abortButton)(*.buttons)

      <.div(select, buttons)
    }
  }

  val Component = ScalaComponent.builder[Props]("ReqTypeSelector")
    .renderBackend[Backend]
    // .configure(Reusability.shouldComponentUpdate)
    .build

  // ===================================================================================================================

  def pxCustomReqTypes(p: Px[Project]): Px[Set[RT]] =
    p.map(_.config.reqTypes.custom.values.toSet)

  def pxChoices(initial: RT, pxCustomReqTypes: Px[Set[RT]]): Px[NonEmptySet[RT]] =
    pxCustomReqTypes
      .map(_.filter(_.live is Live))
      .map(NonEmptySet(initial, _))
}
