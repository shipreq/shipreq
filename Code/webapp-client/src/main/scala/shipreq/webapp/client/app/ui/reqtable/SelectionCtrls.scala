package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import shipreq.base.util.{NonEmptyVector, NonEmptySet}
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.app.ui.{RemoteDataEditor, ProjectWidgets, Modal}
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.TCB
import Cell.Loc

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

  private case class SelSubset(reqs: Vector[Req], groups: Vector[ReqCodeGroup]) {
    def total = reqs.length + groups.length
  }

  private case class SelSubsets(deletable: SelSubset, restorable: SelSubset)
  private val emptySelSubsets = {
    val e = SelSubset(Vector.empty, Vector.empty)
    SelSubsets(e, e)
  }

  private def selSubsets(p: Props): SelSubsets = {
    var remaining = p.sel.legalSelection // because the same sourceId can appear more than once

    if (remaining.isEmpty)
      emptySelSubsets

    else {
      var delReqs   = Vector.newBuilder[Req]
      var delGroups = Vector.newBuilder[ReqCodeGroup]
      var resReqs   = Vector.newBuilder[Req]
      var resGroups = Vector.newBuilder[ReqCodeGroup]

      p.rows foreach { row =>
        val id = row.sourceId
        if (remaining contains id) {
          remaining -= id
          row match {
            case r: GenericReqRow =>
              import GenericReq.ImplicitLiveStatus._
              r.live match {
                case Live => delReqs += r.req
                case Dead => r.req.implicitLiveStatus(p.cfg.customReqTypes) match {
                  case NoImpact      => resReqs += r.req
                  case ReqTypeIsDead => ()
                }
              }
            case r: ReqCodeGroupRow =>
              r.live match {
                case Live => delGroups += r.group
                case Dead => resGroups += r.group
              }
          }
        }
      }

      val d = SelSubset(delReqs.result(), delGroups.result())
      val r = SelSubset(resReqs.result(), resGroups.result())
      SelSubsets(d, r)
    }
  }

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {

      val totalSelected = p.sel.legalSelection.size
      val ss = selSubsets(p)

      def infoText =
        totalSelected match {
          case 0 => "0 items selected."
          case 1 => "1 item selected."
          case n => n.toString + " items selected."
        }

      def addCount(label: String, count: Int, suffix: String = ""): String = {
        var s = label
        if (count != totalSelected)
          s = s"$s ($count/$totalSelected)"
        s + suffix
      }

      val delButton = {
        val d = ss.deletable

        def delReqsAndGroups =
          NonEmptyVector.option(d.reqs).map { rs =>
            def modal = deleteReqsModal(p, rs.mapToNES(_.id), d.groups.map(_.id)(collection.breakOut))
            val count = rs.length + d.groups.length
            <.button(
              ^.onClick --> p.setModal(modal),
              addCount("Delete", count, " →"))
          }

        def delGroupsOnly =
          NonEmptyVector.option(d.groups).map { gs =>
            <.button(
              ^.onClick --> deleteGroupsIO(gs.mapToNES(_.id)),
              addCount("Delete Groups", gs.length))
          }

        def cantDelete =
          <.button(^.disabled := true, "Delete")

        delReqsAndGroups orElse delGroupsOnly getOrElse cantDelete
      }

//      def button(count: Int, label: String, modal: => Modal) =
//        if (count == 0)
//          <.button(
//            ^.disabled := true,
//            label)
//        else
//          <.button(
//            ^.onClick --> p.setModal(modal),
//            addCount(label, count))
//      val resButton = button(restorable.total, "Restore", TEST_MODAL)

      <.div(infoText, delButton)
    }

    val cancel = $.props.flatMap(_ setModal Modal.none)

    def updateRowsIO(cmd: UpdateContentCmd, locs: List[Loc]): Callback = {
      def setRowStates(state: Cell.State) =
        $.props >>= (_.modCellStates(ts => locs.foldLeft(ts)(_.set(_, state))))

      def unlockRows =
        setRowStates(None)

      def uncheckRows =
        $.props >>= { p =>
          val newSel = p.sel clearAll locs.map(_.row)
          p.sel updateFn newSel
        }

      $.props >>= { p =>
        val lockRows = {
          import RemoteDataEditor._
          val locked = Some(StateFor((), Locked, () => defaultRenderLock))
          setRowStates(locked)
        }

        def callServer: Callback = {
          val s = TCB.Success(unlockRows >> uncheckRows)
          val f = (err: String) => TCB.Failure.lazily(
            if (dom.confirm(s"Deletion failed. $err\n\nRetry?"))
              callServer
            else
              unlockRows
          )
          p.saveIO(cmd, s, f)
        }

        lockRows >> p.setModal(None) >> callServer
      }
    }

    def deleteGroupsIO(groups: NonEmptySet[ReqCodeId]): Callback = {
      val cmd = UpdateContentCmd.DeleteReqCodeGroups(groups)
      val locs: List[Loc] =
        cmd.ids.whole.map(id => Loc(Row.ReqCodeGroupRowSourceId(id), None))(collection.breakOut)
      updateRowsIO(cmd, locs)
    }

    def deleteReqsIO(cmd: UpdateContentCmd.DeleteReqs): Callback = {
      def reqLocs = cmd.reqs.whole.iterator.map {
        case i: GenericReqId => Loc(Row.GenericReqRowSourceId(i), None)
      }
      def groupLocs = cmd.reqCodeGroups.iterator.map(i => Loc(Row.ReqCodeGroupRowSourceId(i), None))
      updateRowsIO(cmd, (reqLocs ++ groupLocs).toList)
    }

    def deleteReqsModal(p: Props, reqs: NonEmptySet[ReqId], groups: Set[ReqCodeId]): Modal = {
      val props1 = Deletion.initProps1(p.project, reqs, groups)
      val props = Deletion.makeProps(props1, p.widgets, p.projectText, p.textSearch, deleteReqsIO, cancel)
      Modal(Deletion.Component(props))
    }
  }

  val Component = ReactComponentB[Props]("SelCtrls")
    .renderBackend[Backend]
    .configure(shouldComponentUpdate)
    .build
}
