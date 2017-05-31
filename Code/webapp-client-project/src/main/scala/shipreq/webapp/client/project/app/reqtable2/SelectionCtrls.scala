package shipreq.webapp.client.project.app.reqtable2

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.univeq._
import org.scalajs.dom.window
import scalacss.ScalaCssReact._
import shipreq.base.util.Allow
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.{TextSearch, PlainText}
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.base.feature.AsyncFeature
import shipreq.webapp.client.base.ui.semantic.{Button, Icon}
import shipreq.webapp.client.project.app.Style.reqtable2.{page => *}
import shipreq.webapp.client.project.feature.Modal
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.protocol.ServerCall
import shipreq.webapp.client.project.widgets.{DeletionForm, ProjectWidgets}

/**
  * Provides users with means to apply actions in bulk, across selected requirements.
  *
  * Example:
  *
  *   [Delete (14/15)]  [Restore (1/15)]
  */
object SelectionCtrls {

  final case class Props(sel        : RowSelectionVisible,
                         rows       : Vector[Row],
                         setModal   : Modal.SetFn,
                         project    : Project,
                         widgets    : ProjectWidgets,
                         projectText: PlainText.ForProject,
                         textSearch : TextSearch,
                         updateIO   : ServerCall[UpdateContentCmd],
                         async      : AsyncFeature.Write.D1[Row.SourceId, Nothing]) {
    @inline def render: VdomElement = Component(this)
  }

  final case class Derived(totalSelected: Int,
                           delete       : Option[ActionInfo],
                           restore      : Option[ActionInfo])

  final case class ActionInfo(affects: Int, perform: Callback)

  implicit def reusabilityProps: Reusability[Props] =
    Reusability.byRef || Reusability.caseClass

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Backend($: BackendScope[Props, Unit]) {

    def derive(p: Props): Derived = {
      var total     = 0 // sel.legalSelection.size is O(n) so we increment this as we traverse
      var remaining = p.sel.legalSelection // because the same sourceId can appear more than once in Rows
      var delRq     = Vector.empty[Req]
      var delCG     = Vector.empty[CodeGroup]
      var resRq     = Vector.empty[Req]
      var resCG     = Vector.empty[CodeGroup]
      val reqTypes  = p.project.config.reqTypes

      // Here we have to traverse Rows because
      // 1. we don't have a (r: Row.SourceId => r.Row) lookup. Switch to this approach later if needed elsewhere
      // 2. it's not easy to lookup an ActiveCodeGroup by ReqCodeId which is all you can do with a Project
      for (row <- p.rows) {
        val id = row.sourceId
        if (remaining contains id) {
          remaining -= id
          total += 1
          row match {
            case Row.ForReq(r, Live, _, _, _) =>
              delRq :+= r
            case Row.ForReq(r, Dead, _, _, _) =>
              if (r.allowLiveChange(reqTypes) is Allow)
                resRq :+= r
            case r: Row.ForCodeGroup =>
              r.live match {
                case Live => delCG :+= r.group
                case Dead => resCG :+= r.group
              }
          }
        }
      }
      assert(remaining.isEmpty)

      Derived(
        totalSelected = total,
        delete        = deleteReqsAndCodeGroupsAction(p, delRq, delCG) orElse deleteCodeGroupsAction(delCG),
        restore       = restoreAction(resRq, resCG))
    }

    private val clearModal: Callback =
      $.props.flatMap(_.setModal(None))

    private def deleteReqsAndCodeGroupsAction(p: Props, reqs: Vector[Req], codeGroups: Vector[CodeGroup]): Option[ActionInfo] =
      NonEmptyVector.option(reqs).map { rs =>
        val affects = rs.length + codeGroups.length
        def modal = deleteReqsModal(p, rs.mapToNES(_.id), codeGroups.map(_.id)(collection.breakOut))
        val action = Callback.lazily(p.setModal(modal))
        ActionInfo(affects, action)
      }

    private def deleteReqsModal(p: Props, reqs: NonEmptySet[ReqId], groups: Set[ReqCodeId]): Modal = {
      val props1 = DeletionForm.initProps1(p.project, reqs, groups)
      val props = DeletionForm.makeProps(props1, p.widgets, p.projectText, p.textSearch, deleteReqsIO, clearModal)
      Modal(DeletionForm.Component(props))
    }

    private def deleteReqsIO(cmd: UpdateContentCmd.DeleteReqs): Callback = {
      val sourceIds: List[Row.SourceId] =
        cmd.reqs.iterator.map(Row.SourceId.ForReq)
          .++(cmd.codeGroups.iterator.map(Row.SourceId.ForCodeGroup))
          .toList
      runCmd(cmd, sourceIds)
    }

    private def deleteCodeGroupsAction(codeGroups: Vector[CodeGroup]): Option[ActionInfo] =
      NonEmptyVector.option(codeGroups).map { gs =>
        val affects = gs.length
        val action = Callback.lazily(deleteGroupsIO(gs.mapToNES(_.id)))
        ActionInfo(affects, action)
      }

    private def deleteGroupsIO(codeGroups: NonEmptySet[ReqCodeId]): Callback = {
      val cmd = UpdateContentCmd.DeleteCodeGroups(codeGroups)
      val sourceIds: List[Row.SourceId] = cmd.ids.whole.map(Row.SourceId.ForCodeGroup)(collection.breakOut)
      runCmd(cmd, sourceIds)
    }

    private def restoreAction(reqs: Vector[Req], codeGroups: Vector[CodeGroup]): Option[ActionInfo] = {
      val affects = reqs.length + codeGroups.length
      Option.unless(affects ==* 0) {
        val cmd = UpdateContentCmd.RestoreContent(
          reqs.map(_.id)(collection.breakOut),
          codeGroups.map(_.id)(collection.breakOut))
        ActionInfo(affects, restoreIO(cmd))
      }
    }

    private def restoreIO(cmd: UpdateContentCmd.RestoreContent): Callback = {
      val sourceIds: List[Row.SourceId] =
        cmd.reqs.iterator.map(Row.SourceId.ForReq)
          .++(cmd.codeGroups.iterator.map(Row.SourceId.ForCodeGroup))
          .toList
      runCmd(cmd, sourceIds)
    }

    private def runCmd(cmd: UpdateContentCmd, rows: Iterable[Row.SourceId]): Callback = {
      def setRowStatuses(s: AsyncFeature.State.D0[Nothing]): Callback =
        $.props.flatMap(_.async.setBulk(rows, s))

      def lockRows: Callback =
        setRowStatuses(Some(AsyncFeature.Status.InProgress))

      def unlockRows: Callback =
        setRowStatuses(None)

      def uncheckRows: Callback =
        $.props.flatMap { p =>
          val newSel = p.sel clearAll rows
          p.sel updateFn newSel
        }

      def callServer: Callback = {
        val s = TCB.Success(unlockRows >> uncheckRows)
        val f = (err: String) => TCB.Failure.lazily(
          if (window.confirm(s"Deletion failed. $err\n\nRetry?"))
            callServer
          else
            unlockRows
        )
        $.props.flatMap(_.updateIO(cmd, s, f))
      }

      lockRows >> clearModal >> callServer
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def render(p: Props) = {
      val derived = derive(p)

      var result: VdomTag = <.div

      def addButton(name: String, icon: Icon, a: ActionInfo): Unit = {
        val label: String =
          if (a.affects ==* derived.totalSelected)
            name
          else
            s"$name (${a.affects}/${derived.totalSelected})"

        val button: VdomTag =
          Button(tipe = Button.Type.IconAndText(icon, label)).tag(
            *.actionCtrlButton,
            ^.onClick --> a.perform)

        result = result(button)
      }

      derived.delete.foreach(addButton(UiText.Life.delete, Icon.Trash, _))
      derived.restore.foreach(addButton(UiText.Life.restore, Icon.Undo, _))

      result
    }
  }

  val Component = ScalaComponent.builder[Props]("SelectionCtrls")
    .renderBackend[Backend]
    .configure(shouldComponentUpdate)
    .build
}
