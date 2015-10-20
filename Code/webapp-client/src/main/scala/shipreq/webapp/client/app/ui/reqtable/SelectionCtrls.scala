package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.client.lib.TCB
import scalajs.js
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.ui.{RemoteDataEditor, ProjectWidgets, Modal}
import shipreq.webapp.client.data.DataReusability._

object SelectionCtrls {

  case class Props(sel          : RowSelectionVisible,
                   cfg          : ProjectConfig,
                   rows         : Rows,
                   setModal     : Modal.SetFn,
                   project      : Project,
                   widgets      : ProjectWidgets,
                   projectText  : PlainText.ForProject,
                   textSearch   : TextSearch,
                   saveIO       : CallServer[UpdateContentCmd],
                   modCellStates: Cell.ModifyFn)

  // These two are only used in callbacks so are always reusable
  private implicit def reusabilityPlainText : Reusability[PlainText.ForProject] = Reusability.always
  private implicit def reusabilityTextSearch: Reusability[TextSearch]           = Reusability.always

  implicit def reuseProps: Reusability[Props] = Reusability.caseClass

  private class DelRest {
    val reqs = new js.Array[Req]
    val rcgs = new js.Array[ReqCodeGroup]
    def total = reqs.length + rcgs.length
  }

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {

      val totalSelected = p.sel.visibleSelection.size

      var deletable = new DelRest
      var restorable = new DelRest

      if (totalSelected != 0) {
        var remaining = p.sel.visibleSelection // because the same sourceId can appear more than once
        p.rows foreach { row =>
          val id = row.sourceId
          if (remaining contains id) {
            remaining -= id
            row match {
              case r: GenericReqRow =>
                r.live match {
                  case Live => deletable.reqs push r.req
                  case Dead =>
                    if (r.req.isRestorable(p.cfg.customReqTypes))
                      restorable.reqs push r.req
                }
              case r: ReqCodeGroupRow =>
                r.live match {
                  case Live => deletable.rcgs push r.group
                  case Dead => restorable.rcgs push r.group
                }
            }
          }
        }
      }

      def infoText =
        totalSelected match {
          case 0 => "0 items selected."
          case 1 => "1 item selected."
          case n => n.toString + " items selected."
        }

      def TEST_MODAL =
        Modal(<.p("Press  to return.", ^.onClick --> cancel))

      def button(count: Int, label: String, modal: => Modal) =
        if (count == 0)
          <.button(
            ^.disabled := true,
            label)
        else
          <.button(
            ^.onClick --> p.setModal(modal),
            if (count == totalSelected) label else s"$label ($count)")


      val delButton = button(deletable.total, "Delete", deleteModal(p, deletable))
      val resButton = button(restorable.total, "Restore", TEST_MODAL)

      <.div(infoText, delButton, resButton)
    }

    val cancel = $.props.flatMap(_ setModal Modal.none)

    def deleteModal(p: Props, selected: DelRest): Modal = {
      val perform: UpdateContentCmd.DeleteReqs => Callback =
        dr => $.props >>= { p =>

          val locs = {
            import Cell.Loc

            def reqLocs = dr.reqs.whole.iterator.map {
              case i: GenericReqId => Loc(Row.GenericReqRowSourceId(i), None)
            }

            def groupLocs = dr.reqCodeGroups.iterator.map(i => Loc(Row.ReqCodeGroupRowSourceId(i), None))

            (reqLocs ++ groupLocs).toList
          }

          def setRowStates(state: Cell.State) =
            p.modCellStates(ts => locs.foldLeft(ts)(_.set(_, state)))

          val lockRows = {
            import RemoteDataEditor._
            val locked = Some(StateFor((), Locked, () => defaultRenderLock))
            setRowStates(locked)
          }

          def unlockRows =
            setRowStates(None)

          def callServer: Callback = {
            val s = TCB.Success(unlockRows)
            val f = (err: String) => TCB.Failure.lazily(
              if (dom.confirm(s"Deletion failed. $err\n\nRetry?"))
                callServer
              else
                unlockRows
            )
            p.saveIO(dr, s, f)
          }

          // TODO Should also deselect all
          lockRows >> callServer >> p.setModal(None)
        }

      val props1 = Deletion.initProps1(p.project, selected.reqs, selected.rcgs.map(_.id)(collection.breakOut))
      val props = Deletion.makeProps(props1, p.widgets, p.projectText, p.textSearch, perform, cancel)
      Modal(Deletion.Component(props))
    }
  }

  val Component = ReactComponentB[Props]("SelCtrls")
    .renderBackend[Backend]
    .configure(shouldComponentUpdate)
    .build
}
