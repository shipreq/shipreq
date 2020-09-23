package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util._
import shipreq.webapp.base.data.{ExternalPubid, ReqTypes}
import shipreq.webapp.base.ui.Toast
import shipreq.webapp.client.project.app.pages.content.reqtable.NewStuff.State
import shipreq.webapp.client.project.feature.CreateFeature
import shipreq.webapp.client.project.feature.CreateFeature.RowKey
import shipreq.webapp.client.project.feature.SavedViewFeature.ColumnPlus
import shipreq.webapp.client.project.widgets.{NewReqButton, ProjectWidgets}

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

final class NewStuff(state        : State,
                     modState     : ModFn[State],
                     routerCtl    : RouterCtl[ExternalPubid],
                     pw           : ProjectWidgets.NoCtx,
                     toast        : Toast,
                     reqTypes     : ReqTypes,
                     allowRCG     : Permission,
                     create       : CreateFeature.ReadWrite.ForProject,
                     activeColumns: NonEmptyVector[ColumnPlus]) {

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
          state      = Some(s),
          reqTypes   = reqTypes,
          allowRCG   = allowRCG,
          pw         = pw,
          callbacks  = Some(buttonCallbacks),
          inProgress = false,
        )

        // If what we thought was open is no longer acceptable, proceed as if closed
        if (b.dropdownProps.selected.forall(_ !=* s))
          b = b.copy(state = None)
        b

      case State.Closed(o) =>
        NewReqButton.Props(
          state      = o,
          reqTypes   = reqTypes,
          allowRCG   = allowRCG,
          pw         = pw,
          callbacks  = Some(buttonCallbacks),
          inProgress = false,
        )
    }

  private val cancel: Callback =
    modState.modState(_.close)

  val form: Option[VdomElement] =
    state match {
      case State.Open(s) if buttonProps.state.isDefined =>

        s match {

          case r: RowKey.CodeGroup.type =>
            Some(NewForm.ForCodeGroup.Props((), activeColumns, create(r), routerCtl, toast, cancel).render)

          case r: RowKey.GenericReq =>
            reqTypes.custom.get(r.reqTypeId).map { rt =>
              NewForm.ForGenericReq.Props(rt, activeColumns, create(r), routerCtl, toast, cancel).render
            }

          case r: RowKey.UseCase.type =>
            Some(NewForm.ForUseCase.Props((), activeColumns, create(r), routerCtl, toast, cancel).render)

          case RowKey.ManualIssue =>
            None
        }

      case _ =>
        None
    }

}
