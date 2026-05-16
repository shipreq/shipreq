package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util._
import shipreq.webapp.client.project.app.pages.content.reqtable.NewStuff.State
import shipreq.webapp.client.project.feature.CreateFeature
import shipreq.webapp.client.project.feature.CreateFeature.RowKey
import shipreq.webapp.client.project.feature.SavedViewFeature.ColumnPlus
import shipreq.webapp.client.project.widgets.{NewReqButton, ProjectWidgets}
import shipreq.webapp.member.feature.PreviewFeature
import shipreq.webapp.member.project.data.{ExternalPubid, Project}
import shipreq.webapp.member.project.text.{PlainText, TextSearch}
import shipreq.webapp.member.ui.Toast

/**
  * Unified, convenience interface to both [[NewReqButton]] and [[NewForm]].
  */
object NewStuff {

  sealed abstract class State {
    def close: State
    def toggle(r: RowKey): State
    def setSelection(r: RowKey): State
  }

  object State {
    final case class Open(selected: RowKey) extends State {
      override def close =
        Closed(Some(selected))

      override def setSelection(r: RowKey) =
        Open(r)

      override def toggle(r: RowKey) =
        Closed(Some(r))
    }

    final case class Closed(selected: Option[RowKey]) extends State {
      override def close =
        this

      override def setSelection(r: RowKey) =
        Closed(Some(r))

      override def toggle(r: RowKey) =
        Open(r)
    }

    def init: State =
      State.Closed(None)
  }
}

final class NewStuff(previewRW     : PreviewFeature.ReadWrite.Composite[CreateFeature.PreviewId],
                     project       : Project,
                     plainText     : PlainText.ForProject.AnyCtx,
                     textSearch    : TextSearch,
                     projectWidgets: ProjectWidgets.NoCtx,
                     state         : State,
                     modState      : ModFn[State],
                     routerCtl     : RouterCtl[ExternalPubid],
                     toast         : Toast,
                     allowRCG      : Permission,
                     create        : CreateFeature.ReadWrite.ForProject,
                     activeColumns : NonEmptyVector[ColumnPlus],
                     editability   : Permission) {

  private val buttonCallbacks: Reusable[NewReqButton.Callbacks] =
    modState.map { f =>

      def selectRow(next: RowKey): Callback = {
        val prev: Option[RowKey] =
          state match {
            case State.Open(k)   => Some(k)
            case State.Closed(o) => o
          }
        val retainState = create.selectWithRetention(prev, next)
        val select      = f.modStateAsync(_.setSelection(next))
        (retainState >> select).toCallback
      }

      NewReqButton.Callbacks(
        select = selectRow,
        click = c => f.modState(_.toggle(c.value)).unless_(c.targetsNewTab_?))
    }

  val buttonProps: NewReqButton.Props =
    state match {
      case State.Open(s) =>
        var b = NewReqButton.Props(
          state       = Some(s),
          reqTypes    = project.config.reqTypes,
          allowRCG    = allowRCG,
          pw          = projectWidgets,
          callbacks   = Some(buttonCallbacks),
          inProgress  = false,
          editability = editability,
        )

        // If what we thought was open is no longer acceptable, proceed as if closed
        if (b.dropdownProps.selected.forall(_ !=* s))
          b = b.copy(state = None)
        b

      case State.Closed(o) =>
        NewReqButton.Props(
          state       = o,
          reqTypes    = project.config.reqTypes,
          allowRCG    = allowRCG,
          pw          = projectWidgets,
          callbacks   = Some(buttonCallbacks),
          inProgress  = false,
          editability = editability,
        )
    }

  private val cancel: Reusable[Callback] =
    modState.map(_.modState(_.close))

  val form: Option[VdomElement] =
    state match {
      case State.Open(s) if buttonProps.state.isDefined =>

        s match {

          case r: RowKey.CodeGroup.type =>
            Some(
              NewForm.ForCodeGroup.Props(
                previewRW      = previewRW,
                project        = project,
                plainText      = plainText,
                textSearch     = textSearch,
                projectWidgets = projectWidgets,
                input          = (),
                activeColumns  = activeColumns,
                createFeature  = create(r),
                routerCtl      = routerCtl,
                toast          = toast,
                close          = cancel,
              ).render
            )

          case r: RowKey.GenericReq =>
            project.config.reqTypes.custom.get(r.reqTypeId).map { rt =>
              NewForm.ForGenericReq.Props(
                previewRW      = previewRW,
                project        = project,
                plainText      = plainText,
                textSearch     = textSearch,
                projectWidgets = projectWidgets,
                input          = rt,
                activeColumns  = activeColumns,
                createFeature  = create(r),
                routerCtl      = routerCtl,
                toast          = toast,
                close          = cancel,
              ).render
            }

          case r: RowKey.UseCase.type =>
            Some(
              NewForm.ForUseCase.Props(
                previewRW      = previewRW,
                project        = project,
                plainText      = plainText,
                textSearch     = textSearch,
                projectWidgets = projectWidgets,
                input          = (),
                activeColumns  = activeColumns,
                createFeature  = create(r),
                routerCtl      = routerCtl,
                toast          = toast,
                close          = cancel,
              ).render
            )

          case RowKey.ManualIssue =>
            None
        }

      case _ =>
        None
    }

}
