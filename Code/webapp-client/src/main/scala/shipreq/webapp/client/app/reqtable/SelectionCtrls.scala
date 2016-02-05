package shipreq.webapp.client.app.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom
import shipreq.base.util.{NonEmptyVector, NonEmptySet}
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.data.TCB
import shipreq.webapp.client.feature.AsyncActionFeature.Locked
import shipreq.webapp.client.feature.Modal
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.widgets.high.ProjectWidgets

/**
  * Renders a bar that provides the user with information and action-buttons pertaining to the rows selected the
  * ReqTable.
  *
  * Example:
  *
  *   15 items selected.  [Delete (14/15) →]  [Restore (1/15)]
  */
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
                   async        : AsyncState.FeatureAnon)

  // These two are only used in callbacks so are always reusable
  private implicit def reusabilityPlainText : Reusability[PlainText.ForProject]  = Reusability.always
  private implicit def reusabilityTextSearch: Reusability[TextSearch]            = Reusability.always

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

  def locsOfReqs(ids: Iterator[ReqId]): Iterator[AsyncState.Row] =
    ids.map {
      case i: GenericReqId => Row.GenericReqRowSourceId(i)
    }

  def locsOfGroups(ids: Iterator[ReqCodeId]): Iterator[AsyncState.Row] =
    ids.map(Row.ReqCodeGroupRowSourceId)

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

      val deleteButton = {
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

      val restoreButton = {
        val r = ss.restorable
        if (r.total == 0)
          <.button(^.disabled := true, "Restore")
        else {
          def cmd = UpdateContentCmd.RestoreContent(
            r.reqs.map(_.id)(collection.breakOut),
            r.groups.map(_.id)(collection.breakOut))
          <.button(
            ^.onClick --> restoreIO(cmd),
            addCount("Restore", r.total))
        }
      }

      <.div(infoText, deleteButton, restoreButton)
    }

    val cancel = $.props.flatMap(_ setModal Modal.none)

    def deleteGroupsIO(groups: NonEmptySet[ReqCodeId]): Callback = {
      val cmd = UpdateContentCmd.DeleteReqCodeGroups(groups)
      val locs = locsOfGroups(cmd.ids.iterator).toList
      callRemoteAndUpdateRows(cmd, locs)
    }

    def deleteReqsIO(cmd: UpdateContentCmd.DeleteReqs): Callback = {
      val locs = locsOfReqs(cmd.reqs.iterator) ++ locsOfGroups(cmd.reqCodeGroups.iterator)
      callRemoteAndUpdateRows(cmd, locs.toList)
    }

    def deleteReqsModal(p: Props, reqs: NonEmptySet[ReqId], groups: Set[ReqCodeId]): Modal = {
      val props1 = Deletion.initProps1(p.project, reqs, groups)
      val props = Deletion.makeProps(props1, p.widgets, p.projectText, p.textSearch, deleteReqsIO, cancel)
      Modal(Deletion.Component(props))
    }

    def restoreIO(cmd: UpdateContentCmd.RestoreContent): Callback = {
      val locs = locsOfReqs(cmd.reqs.iterator) ++ locsOfGroups(cmd.reqCodeGroups.iterator)
      callRemoteAndUpdateRows(cmd, locs.toList)
    }

    private def callRemoteAndUpdateRows(cmd: UpdateContentCmd, rows: List[AsyncState.Row]): Callback = {
      val async = $.props.map(_.async).runNow()

      def lockRows: Callback =
        async.setRowStatuses(rows, Some(Locked))

      def unlockRows: Callback =
        async.setRowStatuses(rows, None)

      def uncheckRows: Callback =
        $.props >>= { p =>
          val newSel = p.sel clearAll rows
          p.sel updateFn newSel
        }

      $.props >>= { p =>
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

  }

  val Component = ReactComponentB[Props]("SelCtrls")
    .renderBackend[Backend]
    .configure(shouldComponentUpdate)
    .build
}
