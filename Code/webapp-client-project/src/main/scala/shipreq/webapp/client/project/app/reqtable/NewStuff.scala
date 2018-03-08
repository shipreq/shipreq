package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import shipreq.base.util._
import shipreq.webapp.base.data.ReqTypes
import shipreq.webapp.client.project.feature.CreateFeature
import shipreq.webapp.client.project.feature.CreateFeature.RowKey
import NewStuff.State

/**
  * Unified, convenience interface to both [[NewButton]] and [[NewForm]].
  */
object NewStuff {

  sealed abstract class State

  object State {
    final case class Open(selected: RowKey) extends State
    final case class Closed(selected: Option[RowKey]) extends State

    def init: State =
      State.Closed(None)
  }

}

final class NewStuff(state        : State,
                     setState     : SetFn[State],
                     reqTypes     : ReqTypes,
                     allowRCG     : Permission,
                     defaultType  : Option[RowKey],
                     create       : CreateFeature.ReadWrite.ForProject,
                     activeColumns: NonEmptyVector[ColumnPlus]) {

  private val buttonUpdate: Reusable[NewButton.Update] =
    setState.map(f =>
      NewButton.Update(
        s => f.setState(State.Closed(s)),
        s => f.setState(State.Open(s))))

  val buttonProps: NewButton.Props =
    state match {
      case State.Open(s) =>
        var b = NewButton.Props(Some(s), reqTypes, allowRCG, defaultType, None)
        // If what we thought was open is no longer acceptable, proceed as if closed
        if (!b.selected.exists(_ ==* s))
          b = b.copy(update = Some(buttonUpdate))
        b

      case State.Closed(o) =>
        NewButton.Props(o, reqTypes, allowRCG, defaultType, Some(buttonUpdate))
    }

  val form: Option[VdomElement] =
    state match {
      case State.Open(s) if buttonProps.update.isEmpty =>

        val cancel: Callback =
          setState.setState(State.Closed(Some(s)))

        val props: NewForm#Props =
          s match {

            case r@RowKey.CodeGroup =>
              NewForm.ForCodeGroup.Props((), activeColumns, create(r), cancel)

            case r: RowKey.GenericReq =>
              val rt = reqTypes.custom.need(r.reqTypeId)
              NewForm.ForGenericReq.Props(rt, activeColumns, create(r), cancel)

            case r@RowKey.UseCase =>
              NewForm.ForUseCase.Props((), activeColumns, create(r), cancel)
        }

        Some(props.render)

      case _ =>
        None
    }

}
