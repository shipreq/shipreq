package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import shipreq.base.util._
import shipreq.webapp.base.data.{ExternalPubid, ReqTypes}
import shipreq.webapp.base.ui.Toast
import shipreq.webapp.client.project.app.pages.content.reqtable.NewStuff.State
import shipreq.webapp.client.project.feature.CreateFeature
import shipreq.webapp.client.project.feature.CreateFeature.RowKey
import shipreq.webapp.client.project.feature.SavedViewFeature.ColumnPlus
import shipreq.webapp.client.project.widgets.ProjectWidgets

/**
  * Unified, convenience interface to both [[NewButton]] and [[NewForm]].
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

  private val buttonUpdate: Reusable[NewButton.Update] =
    modState.map(f =>
      NewButton.Update(
        select = s => f.modState(_.setSelection(s)),
        click  = s => f.modState(_.toggle(s))))

  val buttonProps: NewButton.Props =
    state match {
      case State.Open(s) =>
        var b = NewButton.Props(Some(s), reqTypes, allowRCG, pw, Some(buttonUpdate))
        // If what we thought was open is no longer acceptable, proceed as if closed
        if (b.dropdownProps.selected.forall(_ !=* s))
          b = b.copy(state = None)
        b

      case State.Closed(o) =>
        NewButton.Props(o, reqTypes, allowRCG, pw, Some(buttonUpdate))
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
