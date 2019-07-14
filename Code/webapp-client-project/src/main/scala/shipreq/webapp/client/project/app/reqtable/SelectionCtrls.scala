package shipreq.webapp.client.project.app.reqtable

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.univeq._
import org.scalajs.dom.window
import scalacss.ScalaCssReact._
import shipreq.base.util.{Allow, ErrorMsg}
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.semantic.{Button, Icon}
import shipreq.webapp.client.project.app.Style.reqtable.{page => *}
import shipreq.webapp.client.project.feature.{DeletionFeature, Modal}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

/**
  * Provides users with means to apply actions in bulk, across selected requirements.
  *
  * Example:
  *
  *   [Delete (14/15)]  [Restore (1/15)]
  */
object SelectionCtrls {

  final case class Props(sel       : RowSelectionVisible,
                         rows      : Vector[Row],
                         setModal  : Modal.SetFn,
                         project   : Project,
                         widgets   : ProjectWidgets.NoCtx,
                         textSearch: TextSearch,
                         updateIO  : ServerSideProcInvoker[UpdateContentCmd, ErrorMsg, Any],
                         async     : AsyncFeature.Write.D1[Row.SourceId, Nothing]) {
    @inline def render: VdomElement = Component(this)
  }

  final case class Derived(totalSelected    : Int,
                           deleteReqs       : Option[ActionInfo],
                           deleteCodeGroups : Option[ActionInfo],
                           restoreReqs      : Option[ActionInfo],
                           restoreCodeGroups: Option[ActionInfo])

  final case class ActionInfo(affects: Int, perform: Callback)

  implicit def reusabilityProps: Reusability[Props] =
    Reusability.byRef || Reusability.derive

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Backend($: BackendScope[Props, Unit]) {

    def derive(p: Props): Derived = {
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
          row match {
            case Row.ForReq(r, Live, _, _, _, _) =>
              delRq :+= r
            case Row.ForReq(r, Dead, _, _, _, _) =>
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
        totalSelected     = p.sel.legalSelectionSize,
        deleteReqs        = deleteReqs.action(p, delRq),
        deleteCodeGroups  = deleteCodeGroups.action(delCG),
        restoreReqs       = restoreReqs.action(p, resRq),
        restoreCodeGroups = restoreCodeGroups.action(resCG))
    }

    private val clearModal: Callback =
      $.props.flatMap(_.setModal(None))

    private object deleteReqs {
      def action(p: Props, reqs: Vector[Req]): Option[ActionInfo] =
        NonEmptyVector.option(reqs).map { rs =>
          val affects = rs.length
          def modalValue = modal(p, rs.mapToNES(_.id))
          val action = Callback.lazily(p.setModal(modalValue))
          ActionInfo(affects, action)
        }

      private def modal(p: Props, reqs: NonEmptySet[ReqId]): Modal = {
        val data = DeletionFeature.deletionData(p.project, reqs)
        val props = DeletionFeature.DeletionFormProps(data, p.widgets, p.textSearch, io, clearModal)
        Modal(props.render)
      }

      private def io(cmd: UpdateContentCmd.DeleteReqs): Callback = {
        val sourceIds: List[Row.SourceId] =
          cmd.reqs.iterator.map(Row.SourceId.ForReq)
            .++(cmd.codeGroups.iterator.map(Row.SourceId.ForCodeGroup))
            .toList
        runCmd(cmd, sourceIds)
      }
    }

    private object restoreReqs {
      def action(p: Props, reqs: Vector[Req]): Option[ActionInfo] =
        NonEmptyVector.option(reqs).map { rs =>
          val affects = rs.length
          def modalValue = modal(p, rs.mapToNES(_.id))
          val action = Callback.lazily(p.setModal(modalValue))
          ActionInfo(affects, action)
        }

      private def modal(p: Props, reqs: NonEmptySet[ReqId]): Modal = {
        val data = DeletionFeature.restorationData(p.project, reqs)
        val props = DeletionFeature.RestorationFormProps(data, p.widgets, io, clearModal)
        Modal(props.render)
      }

      private def io(cmd: UpdateContentCmd.RestoreContent): Callback = {
        val sourceIds: List[Row.SourceId] =
          cmd.reqs.iterator.map(Row.SourceId.ForReq)
            .++(cmd.codeGroups.iterator.map(Row.SourceId.ForCodeGroup))
            .toList
        runCmd(cmd, sourceIds)
      }
    }


    private object deleteCodeGroups {
      def action(codeGroups: Vector[CodeGroup]): Option[ActionInfo] =
        NonEmptyVector.option(codeGroups).map { gs =>
          val affects = gs.length
          val action = Callback.lazily(io(gs.mapToNES(_.id)))
          ActionInfo(affects, action)
        }

      private def io(codeGroups: NonEmptySet[ReqCodeGroupId]): Callback = {
        val cmd = UpdateContentCmd.DeleteCodeGroups(codeGroups)
        val sourceIds: List[Row.SourceId] = cmd.ids.whole.map(Row.SourceId.ForCodeGroup)(collection.breakOut)
        runCmd(cmd, sourceIds)
      }
    }

    private object restoreCodeGroups {
      def action(codeGroups: Vector[CodeGroup]): Option[ActionInfo] =
        NonEmptyVector.option(codeGroups).map { gs =>
          val affects = gs.length
          val action = Callback.lazily(io(gs.mapToNES(_.id)))
          ActionInfo(affects, action)
        }

      private def io(codeGroups: NonEmptySet[ReqCodeGroupId]): Callback = {
        val cmd = UpdateContentCmd.RestoreContent(Set.empty, codeGroups.whole)
        val sourceIds: List[Row.SourceId] = cmd.codeGroups.map(Row.SourceId.ForCodeGroup)(collection.breakOut)
        runCmd(cmd, sourceIds)
      }
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
        val s = unlockRows >> uncheckRows
        val f = (err: ErrorMsg) => Callback.lazily(
          if (window.confirm(s"Deletion failed. ${err.value}\n\nRetry?"))
            callServer
          else
            unlockRows
        )
        $.props.flatMap(_.updateIO(cmd, _ => s, f))
      }

      lockRows >> clearModal >> callServer
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    def render(p: Props) = {
      val derived = derive(p)

      var result: VdomTag = <.div

      def addButton(name: String, icon: Icon)(a: ActionInfo): Unit = {
        val label: String =
          if (a.affects ==* derived.totalSelected)
            name
          else
            s"$name (${a.affects}/${derived.totalSelected})"

        val button: VdomTag =
          <.div(*.actionCtrlButtonWrap,
            Button(tipe = Button.Type.IconAndText(icon, label)).tag(^.onClick --> a.perform))

        result = result(button)
      }

      (derived.deleteReqs, derived.deleteCodeGroups) match {
        case (None   , None   ) => ()
        case (Some(d), None   ) => addButton(UiText.Life.delete + "…"    , Icon.Trash)(d)
        case (None   , Some(d)) => addButton(UiText.Life.delete          , Icon.Trash)(d)
        case (Some(r), Some(c)) => addButton(UiText.Life.deleteReqs + "…", Icon.Trash)(r)
                                   addButton(UiText.Life.deleteCodeGroups, Icon.Trash)(c)
      }

      (derived.restoreReqs, derived.restoreCodeGroups) match {
        case (None   , None   ) => ()
        case (Some(d), None   ) => addButton(UiText.Life.restore + "…"    , Icon.Undo)(d)
        case (None   , Some(d)) => addButton(UiText.Life.restore          , Icon.Undo)(d)
        case (Some(r), Some(c)) => addButton(UiText.Life.restoreReqs + "…", Icon.Undo)(r)
                                   addButton(UiText.Life.restoreCodeGroups, Icon.Undo)(c)
      }

      result
    }
  }

  val Component = ScalaComponent.builder[Props]("SelectionCtrls")
    .renderBackend[Backend]
    .configure(shouldComponentUpdate)
    .build
}
