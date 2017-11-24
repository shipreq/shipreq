package shipreq.webapp.client.project.feature.deletion

import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd.RestoreContent
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon, Table}
import shipreq.webapp.client.project.app.Style.{restorationForm => *}
import shipreq.webapp.client.project.app.TestMarker
import shipreq.webapp.client.project.feature.Selection
import shipreq.webapp.client.project.widgets.{ProjectWidgets, Widgets}

object RestorationForm {
  import DeletionRestorationLogic.{Data, ReqRow}

  final case class Props(data   : Data,
                         widgets: ProjectWidgets.NoCtx,
                         perform: RestoreContent => Callback,
                         cancel : Callback) {
    def render: VdomElement = Component(this)
  }

  type State = Selection[ReqId]

  def stateInit(p: Props): State =
    Selection(p.data.initialReqs)

  final class Backend($: BackendScope[Props, State]) {
    private val setReqSel = Reusable.fn.state($).set

    private def renderReqTable(p: Props, selectedReqs: State): VdomElement =
      SharedUI.reqTable(
        Restore,
        p.data.project,
        p.widgets,
        p.data.actionableReqs,
        selectedReqs,
        selectedReqs.updateBy(setReqSel).legal(p.data.optionalReqIds),
        *.reqTableRow(_))

    private val cancelButton: VdomTag =
      Button(
        tipe = Button.Type.BasicIconAndText(Icon.Remove, UiText.buttonAbortChange),
        colour = Colour.Black
      ).tag(^.onClick --> $.props.flatMap(_.cancel))

    def render(p: Props, selectedReqs: State): VdomElement = {
      assert(p.data.actionableGroups.isEmpty,
        "Since proper UI/UX implementation, Restoration no longer accepts deletable code-groups")

      val commit: Option[Callback] =
        for {
          reqs ← NonEmptySet.option(selectedReqs.selected)
        } yield p.perform(RestoreContent(reqs.whole, Set.empty))

      val restoreButton: VdomTag =
        Button(
          tipe = Button.Type.BasicIconAndText(Icon.Undo, UiText.Life.restore),
          state = Button.State.enabledWhen(commit.isDefined),
          colour = Colour.Green
        ).tag(^.onClick -->? commit)

      <.main(
        *.main,
        TestMarker.restorationForm.tagMod,
        <.h2("You are about to restore the following requirements:"),
        <.section(
          <.div(*.reqHelp, "In addition to those you selected, implied requirements are also presented with exclusively-implied requirements auto-selected for restoration."),
          renderReqTable(p, selectedReqs)),
        <.div(*.bottomSection,
          cancelButton,
          <.div(*.buttonGap), // curse Semantic UI!
          restoreButton))
    }
  }

  val Component = ScalaComponent.builder[Props]("Restoration")
    .initialStateFromProps(stateInit)
    .renderBackend[Backend]
    .build

}
