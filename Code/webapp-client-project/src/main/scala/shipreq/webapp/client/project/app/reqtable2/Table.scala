package shipreq.webapp.client.project.app.reqtable2

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import scalacss.ScalaCssReact._
import shipreq.base.util.{Applicable, NotApplicable}
import shipreq.webapp.base.data._
import shipreq.webapp.client.base.data.Plain
import shipreq.webapp.client.base.feature.AsyncFeature
import shipreq.webapp.client.base.lib.DomUtil._
import shipreq.webapp.client.base.ui.{EditTheme, semantic}
import shipreq.webapp.client.project.app.Style.reqtable2.{table => *}
import shipreq.webapp.client.project.feature.{EditorFeature, Selection}
import shipreq.webapp.client.project.widgets.{DragToReorder, ProjectWidgets, ViewReq}
import shipreq.webapp.client.project.lib.DataReusability._
import EditorFeature.FieldKey

object Table {

  object Whole {

    final case class Props(rows       : Vector[Row],
                           cols       : NonEmptyVector[ColumnPlus],
                           selection  : RowSelectionVisible,
                           editor     : EditorFeature.ReadWrite.ForProject,
                           rowAsync   : AsyncFeature.Read.D1[Row.SourceId, String],
                           config     : ProjectConfig,
                           pw         : ProjectWidgets,
                           modSettings: ModFn[TableSettings]) {
      @inline def render = Component(this)
    }

    implicit val reusabilityProps: Reusability[Props] =
      Reusability.caseClass

    final class Backend($: BackendScope[Props, Unit]) {

      val pxProjectWidgets = Px.props($).map(_.pw).withReuse.manualRefresh
      val pxProjectConfig  = Px.props($).map(_.config).withReuse.manualRefresh

      val pxPubidFmt: Px[ProjectWidgets#PubidFormat] =
        pxProjectWidgets.map(_.PubidFormat(Plain, *.pubidColumnValue(_), titleFn = _ => None))

      val pxApplicability: Px[Applicability[Column, Row]] =
        pxProjectConfig.map(cfg => Row.applicability(cfg.applicability))

      def render(p: Props): VdomElement = {
        pxProjectWidgets.refresh()
        pxProjectConfig.refresh()

        val header =
          Header.Component(
            Header.Props(
              p.cols,
              p.selection,
              p.modSettings.map(f => cs => f(_ setColumns cs.map(_.column))),
              p.modSettings.map(f => c => f(TableSettings.order.modify(_ want c.column)))))

        val reqViewInputs: ReqRow.ViewInput =
          (p.config.reqTypes, p.pw, pxPubidFmt.value())

        val renderRows: VdomArray =
          p.rows.toVdomArray { genericRow =>
            val rowAsync = p.rowAsync(genericRow.sourceId)
            val selection = p.selection(genericRow.sourceId)
            val applicability = pxApplicability.value()

            genericRow match {
              case row: Row.ForReq =>
                ReqRow.Props(
                  row,
                  reqViewInputs,
                  p.editor.forReq(row.req.id),
                  p.cols,
                  applicability,
                  rowAsync,
                  selection,
                ).render

              case row: Row.ForCodeGroup =>
                CodeGroupRow.Props(
                  row,
                  p.pw,
                  p.editor.forCodeGroup(row.reqCodeId),
                  p.cols,
                  applicability,
                  rowAsync,
                  selection,
                ).render
            }
          }

        semantic.Table.celledCompactUnstackable(
          *.table,
          header,
          <.tbody(renderRows))
      }
    }

    val Component = ScalaComponent.builder[Props]("ReqTable")
      .renderBackend[Backend]
      .configure(shouldComponentUpdate)
      .build
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private object Header {

    case class Props(cols     : NonEmptyVector[ColumnPlus],
                     selection: RowSelectionVisible,
                     reorder  : NonEmptyVector[ColumnPlus] ~=> Callback,
                     clickSort: ColumnPlus ~=> Callback)

    implicit val reusabilityProps = Reusability.caseClass[Props]

    final class Backend($: BackendScope[Props, Unit]) {

      private def setNewOrder(newOrder: Vector[ColumnPlus]): Callback =
        NonEmptyVector.maybe(newOrder, Callback.empty)(newCols =>
          $.props.flatMap(_ reorder newCols))

      private def selColKeyDown(e: ReactKeyboardEventFromHtml): Callback =
        focusKeyHandlers(e)

      private def dataColKeyDown(col: ColumnPlus)(e: ReactKeyboardEventFromHtml): Callback =
        focusKeyHandlers(e) | keyCodeSwitch(e) {
          case KeyCode.Space => $.props.flatMap(_ clickSort col)
        }

      private def renderFn(p: Props, content: DragToReorder.Content[ColumnPlus]): VdomElement = {
        val selectionCell =
          <.th(
            *.selectionColumnHeader,
            ^.onKeyDown ==> selColKeyDown,
            p.selection.total.checkboxAndOnClick)

        val cols =
          content.items.toVdomArray { i =>
            val c = i.data
            val live = c.column match {
              case Column.DeletionReason => Live // Don't render this title with strike-through
              case _                     => c.live
            }
            <.th(
              *.columnHeader(live, i.status),
              i.mod,
              ^.tabIndex   := -1,
              ^.onKeyDown ==> dataColKeyDown(c),
              ^.onClick   --> p.clickSort(c),
              c.name)
          }

        <.thead(
          content.rootMod,
          <.tr(
            selectionCell,
            cols))
      }

      private val columnDND: DragToReorder[ColumnPlus] =
        new DragToReorder(setNewOrder, c => $.props.map(renderFn(_, c)))

      def render(p: Props): VdomElement =
        columnDND.Component(p.cols.whole)
    }

    val Component = ScalaComponent.builder[Props]("Header")
      .renderBackend[Backend]
      .configure(shouldComponentUpdate)
      .build
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private sealed abstract class RowTemplate[
        _RowData   <: Row     : Reusability,
        FK         <: FieldKey,
        _ViewInput            : Reusability,
      ](displayName: String) {

    protected val rowToColumnToEditorField: RowData => Column => Option[FK]

    protected def reusabilityRowEditor: Reusability[RowEditor]

    protected def viewMaker(row: RowData, vi: ViewInput): Column => Reusable[TagMod]

    // ↑ abstract
    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // ↓ concrete

    final type RowData   = _RowData
    final type ViewInput = _ViewInput
    final type RowEditor = EditorFeature.ReadWrite.ForFields[FK]

    case class Props(row          : RowData,
                     viewInput    : ViewInput,
                     editor       : RowEditor,
                     cols         : NonEmptyVector[ColumnPlus],
                     applicability: Applicability[Column, Row],
                     rowAsync     : AsyncFeature.Read.D0[String],
                     selection    : Selection.OneUI[Row.SourceId]) {
      @inline def render = Component.withKey(row.id.key)(this)
    }

    implicit final val reusabilityProps: Reusability[Props] = {
      implicit val a = reusabilityRowEditor
      Reusability.caseClass
    }

    protected final val reusabilityView: Reusability[(RowData, ViewInput, Column)] =
      implicitly

    private val selBase = <.td(*.selectionColumnBody)

    final def render(p: Props): VdomElement = {
      val row = p.row
      val sel = p.selection

      val rowBase = <.tr(*.dataRow(row.live, sel.get))

      def selCellKeyDown(e: ReactKeyboardEventFromHtml): Callback =
        focusKeyHandlers(e)

      val mkViewWhenApplicable: Column => Reusable[TagMod] =
        viewMaker(row, p.viewInput)

      def mkProps(c: Column, ok: Reusable[TagMod] => Cell.Props): Cell.Props =
        p.applicability(row, c) match {
          case Applicable    => ok(mkViewWhenApplicable(c))
          case NotApplicable => Cell.Props.NA
        }

      def renderNormal = {
        def selCell =
          selBase(
            ^.onKeyDown ==> selCellKeyDown,
            sel.onClick,
            sel.checkbox(^.tabIndex := -1))

        val columnToEditorField = rowToColumnToEditorField(p.row)

        def colCells =
          p.cols.whole.toVdomArray { colPlus =>
            val col    = colPlus.column
            def editor = p.editor.optional(columnToEditorField(col))
            val cp     = mkProps(col, Cell.Props(editor, _))
            Cell.Component.withKey(col.key)(cp)
          }

        rowBase(selCell, colCells)
      }

      def renderLocked = {
        def colCells =
          p.cols.whole.toVdomArray { colPlus =>
            val col    = colPlus.column
            def editor = EditorFeature.ReadWrite.ForEditor.doNothing
            val cp     = mkProps(col, Cell.Props(editor, _))
            Cell.Component.withKey(col.key)(cp)
          }

        def lockedSel = <.div(^.cls := "locked", "LOCKED")

        rowBase(selBase(lockedSel), colCells)
      }

      p.rowAsync match {
        case None                                        => renderNormal
        case Some(AsyncFeature.Status.InProgress)        => renderLocked
        case Some(s: AsyncFeature.Status.Failed[String]) =>
          // Currently, whole-row state is only used when a row is being deleted/restored.
          // To save dev-time, if the RPC fails an alert popups asking to retry/cancel, thus this part of the code
          // should only execute when the row is locked. Whole-row editing + failure won't occur.
          dom.console.warn(s.failure)
          rowBase(
            <.td(^.colSpan := (p.cols.length + 1),
              <.div(
                s.failure,
                <.button("Retry", ^.onClick --> s.retry),
                <.button("Abort", ^.onClick --> s.cancel))))
      }
    }

    final val Component = ScalaComponent.builder[Props](displayName)
      .render_P(render)
      .configure(shouldComponentUpdate)
      .build
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object ReqRow extends RowTemplate[
      Row.ForReq,
      FieldKey.ForSomeReq,
      (ReqTypes, ProjectWidgets, ProjectWidgets#PubidFormat),
    ]("ReqRow") {

    override protected val rowToColumnToEditorField =
      _.req.id match {
        case _: GenericReqId => Column.editorFieldGR.getOption
        case _: UseCaseId    => Column.editorFieldUC.getOption
      }

    override protected def reusabilityRowEditor = implicitly

    override protected def viewMaker(row: RowData, vi: ViewInput): Column => Reusable[TagMod] = {
      val (reqTypes, pw, pubidFmt) = vi

      val viewReq = ViewReq.Data(
        row.req,
        row.exp.reqCodes,
        row.mv.tags,
        row.exp.cfTags.getOrElse(_, Vector.empty),
        row.exp.implications.apply,
        row.exp.cfImps.getOrElse(_, Vector.empty),
        Vector.empty, // pastPubids unused
        reqTypes)
        .apply(pw)

      def renderCodes: VdomElement =
        if (row.exp.reqCodeTree.nonEmpty)
          pw.reqCodeTree(row.exp.reqCodeTree)
        else
          viewReq.codes

      val view: Column => TagMod = {
        case Column.CustomField(id)   => viewReq.customField(id)
        case Column.Title             => viewReq.title
        case Column.ReqType           => viewReq.reqType
        case Column.Tags              => viewReq.tags
        case Column.Implications(dir) => viewReq.imps(dir)
        case Column.Code              => renderCodes
        case Column.Pubid             => pubidFmt(row.req)
        case Column.DeletionReason    => viewReq.deletionReason getOrElse `N/A`
      }
      c => Reusable.explicitly((row, vi, c))(reusabilityView).map(_ => view(c))
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private object CodeGroupRow extends RowTemplate[
      Row.ForCodeGroup,
      FieldKey.ForCodeGroup,
      ProjectWidgets,
    ]("CodeGroupRow") {

    override protected val rowToColumnToEditorField =
      _ => Column.editorFieldCG.getOption

    override protected def reusabilityRowEditor = implicitly

    override protected def viewMaker(row: RowData, vi: ViewInput): Column => Reusable[TagMod] = {
      val pw = vi

      def ret(c: Column, view: => TagMod): Reusable[TagMod] =
        Reusable.explicitly((row, vi, c))(reusabilityView).map(_ => view)

      def renderCodes: TagMod =
        row match {
          case Row.ForCodeGroup(_, _, Some(t)) => pw.reqCodeTreeItem(t)
          case Row.ForCodeGroup(_, c, None)    => pw.reqCode(c)
        }

      {
        case _: Column.CustomField
           | _: Column.Implications
           | Column.ReqType
           | Column.Tags
           | Column.Pubid
           | Column.DeletionReason  => reusableNA
        case c@ Column.Title        => ret(c, pw.codeGroupTitle(row.group))
        case c@ Column.Code         => ret(c, renderCodes)
      }
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  private object Cell {

    final case class Props(editor: EditorFeature.ReadWrite.ForEditor[Any], view: Reusable[TagMod])

    object Props {
      implicit val reusability: Reusability[Props] =
        Reusability.caseClass

      val NA = Props(EditorFeature.ReadWrite.ForEditor.doNothing, reusableNA)
    }

    val cellBase = <.td(*.dataCell, ^.tabIndex := -1)

    type $ = ScalaComponent.Lifecycle.RenderScope[Props, Unit, Unit]
    type N = dom.html.TableDataCell

    // def domNode = CallbackTo($.getDOMNode.asInstanceOf[N])

    /**
     * When a Button in the cell is clicked, we still get the event here in which case, the focus is set after the
     * button callback runs, meaning that (because separate modState()s don't compose) we trample the state change made by
     * the button, and replace it with a focus update.
     *
     * Rather than force all cell children to stop propagation of events, we apply so logic here to filter the events to
     * which we react.
     */
    def doesEventTargetCell(e: ReactEventFromHtml): Boolean =
      e.target == e.currentTarget ||
        (try e.target.tabIndex < 0 catch { case _: Throwable => false }) // .tabIndex is undefined from tests

    def onKeyDown(editor: EditorFeature.ReadWrite.ForEditor[Any]): ReactKeyboardEventFromHtml => Callback =
      e => CallbackOption.require(doesEventTargetCell(e)) >> (
        focusKeyHandlers(e) | keyCodeSwitch(e) {
          case KeyCode.F2 => editor.startEdit getOrElse Callback.empty
        }
      )

    def render($: $, p: Props): VdomElement =
      cellBase(
        ^.onKeyDown ==> onKeyDown($.props.editor),
        p.editor.themedRenderOr(p.view))

    val Component = ScalaComponent.builder[Props]("Cell")
      .renderP(render)
      .configure(shouldComponentUpdate)
      .build
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  // Shared

  private implicit val reusabilityApplicability: Reusability[Applicability[Column, Row]] =
    Reusability.byRef

  private val `N/A`: VdomTag =
    <.span(*.`N/A`, "–")

  private val reusableNA: Reusable[TagMod] =
    Reusable.byRef(`N/A`)

  private def moveFocus(cur: dom.html.Element, ↔ : Movement = Movement.None, ↕ : Movement = Movement.None): Callback =
    Callback {
      val cell: dom.html.Element =
        if ("INPUT" == cur.tagName) // Selection checkbox
          cur.parentElement
        else
          cur
      val z = TableCellZipper(cell) move_- ↔ move_| ↕
      val f: dom.html.Element =
        if (z.colIndex == 0)
          z.focus.children(0).domAsHtml // Selection checkbox
        else
          z.focus
      f.focus()
    }

  private def focusKeyHandlers(e: ReactKeyboardEventFromHtml): CallbackOption[Unit] =
    keyCodeSwitch(e) {
      case KeyCode.Up     => moveFocus(e.currentTarget, ↕ = Movement.Prev)
      case KeyCode.Down   => moveFocus(e.currentTarget, ↕ = Movement.Next)
      case KeyCode.Left   => moveFocus(e.currentTarget, ↔ = Movement.Prev)
      case KeyCode.Right  => moveFocus(e.currentTarget, ↔ = Movement.Next)
      case KeyCode.Home   => moveFocus(e.currentTarget, ↔ = Movement.Head)
      case KeyCode.End    => moveFocus(e.currentTarget, ↔ = Movement.Last)
      case KeyCode.Escape => Callback(e.target.blur())
    } | keyCodeSwitch(e, ctrlKey = true) {
      case KeyCode.Home   => moveFocus(e.currentTarget, Movement.Head, Movement.Head)
      case KeyCode.End    => moveFocus(e.currentTarget, Movement.Last, Movement.Last)
    }
}
