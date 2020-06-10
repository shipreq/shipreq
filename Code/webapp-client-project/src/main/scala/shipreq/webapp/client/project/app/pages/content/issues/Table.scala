package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import shipreq.base.util.univeq._
import shipreq.base.util.{ConsolidatedSeq, ErrorMsg, IfApplicable}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.issue.Issues
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.lib.DomUtil
import shipreq.webapp.base.sort.FusedSorters
import shipreq.webapp.base.text.{PlainText, Text}
import shipreq.webapp.base.ui.semantic
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.app.pages.root.Routes
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.lib.EditorNavParent
import shipreq.webapp.client.project.widgets.{NoFilterResults, ProjectWidgets}

object Table {

  final case class StaticProps(pxProject       : Px[Project],
                               pxRenderFeature : Px[RenderFeature.ForProject],
                               pxPlainText     : Px[PlainText.ForProject.NoCtx],
                               pxProjectWidgets: Px[ProjectWidgets.NoCtx],
                               pxFieldNameFn   : Px[FieldId ~=> String],
                               routerCtl       : Routes.RouterCtl,
                               cmdInvoker      : Action.Cmd ~=> Callback) {

    val reusablePxPW  = Reusable.byRef(pxProjectWidgets)
    val pxPubidFormat = pxProjectWidgets.map(_.PubidFormat(Plain, _ => *.pubidColumnValue, titleFn = _ => None))

    val component = ScalaComponent.builder[Props]
      .backend(new Backend(this, _))
      .renderBackend
      .configure(shouldComponentUpdate)
      .build
  }

  final case class Props(issues  : Issues,
                         editor  : EditorFeature.ReadWrite.ForProject,
                         cmdAsync: AsyncFeature.Read.D1[Action.Cmd, ErrorMsg])

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val columns: NonEmptyVector[Column] =
    NonEmptyVector(
      Column.IssueCategory,
      Column.IssueClass,
      Column.Id,
      Column.Title,
      Column.FieldName,
      Column.FieldEditor,
      Column.Actions,
    )

  private val sorter = FusedSorters(
    Sorter.issueCategorySorter,
    Sorter.issueClassSorter,
    Sorter.idSorter,
    Sorter.fieldName,
    Sorter.manualIssueText,
  )

  final class RenderPrep(project      : Project,
                         plainText    : PlainText.ForProject.NoCtx,
                         routerCtl    : Routes.RouterCtl,
                         issues       : Issues,
                         renderFeature: RenderFeature.ForProject) {
    private val sortFn  = sorter.result(new Sorter.Setup(project, plainText))
    private val toRow   = Row.fromIssue(project, renderFeature, routerCtl)
    val rows            = sortFn(issues.vector.iterator.map(toRow)).iterator.toVector
    val csIssueCategory = TableRow.consolidateIssueCategories(rows.iterator.map(_.issueCategoryDesc))
    val csIssueClass    = TableRow.consolidateIssueClasses   (rows.iterator.map(_.issueClassDesc))

    private def groupedRows[A, B](groups: ConsolidatedSeq[Any], f: Row => A)(g: (Int, A) => B): Iterator[B] =
      rows.indices.iterator.map { i =>
        val group = groups.group(i)
        val value = f(rows(i))
        g(group, value)
      }

    val csIds = TableRow.Id.consolidate(groupedRows(csIssueClass, {
      case i: Row.ForReq         => Some(\/-(i.req.id))
      case i: Row.ForRcg         => Some(-\/(i.code))
      case _: Row.ForConfig
         | _: Row.ForManualIssue => None
    })(TableRow.Id.apply))

    val csTitles = TableRow.consolidateTitle(groupedRows(csIds, {
      case i: Row.ForReq         => i.req.title
      case i: Row.ForRcg         => i.rcg.title
      case _: Row.ForConfig      => Text.empty
      case _: Row.ForManualIssue => Text.empty // Don't consolidate manual issue titles
    })((_, _)))
  }

  final class Backend(static: StaticProps, $: BackendScope[Props, Unit]) {
    import static.{component => _, _}

    private val pxIssues     = Px.props($).map(_.issues).withReuse.autoRefresh
    private val pxRenderPrep = Px.apply4(pxProject, pxPlainText, pxIssues, pxRenderFeature)(new RenderPrep(_, _, routerCtl, _, _))

    /** When a user closes a field editor, either
      *
      * 1) there's still an issue in which case [[EditorNavParent]] refocuses the same cell
      * 2) the issue is resolved and the row disappears in which case this will focus a field editor on an adjacent row
      */
    private def focusAlternateRow(p: EditorNavParent.Props, rowIdx: Int): Callback =
      for {
        focus <- CallbackOption.activeHtmlElement.asCallback
        _     <- CallbackOption.require(focus.isEmpty)
        table <- $.getDOMNode.map(_.toHtml).asCBO
      } yield {
        List(rowIdx + 1, rowIdx)
          .iterator
          .flatMap(i => Option(table.querySelector(s":scope>tbody>tr:nth-child($i) [${TableRow.fieldEditorAttr}]")))
          .flatMap(_.domToHtml)
          .nextOption()
          .foreach(_.focus())
      }

    def render(p: Props): VdomElement = {
      val fieldNames = pxFieldNameFn.value()
      val pubidFormat = pxPubidFormat.value()
      val rp = pxRenderPrep.value()
      import rp._

      val header = TableHeader.Props(columns, fieldNames).render

      val body =
        if (rows.isEmpty)
          NoFilterResults.asTableRow(columns.length)
        else
          rows.indices.toVdomArray { rowIdx =>

            val row = rows(rowIdx)

            val editor: Option[Reusable[IfApplicable[EditorNavParent.Props]]] =
              row.editor(p.editor, reusablePxPW).map(
                _.tuple(Reusable.implicitly(rowIdx))
                  .map(_._1.map(p =>
                    p.modEditor(_.onClose(focusAlternateRow(p, rowIdx))))))

            val rowProps = TableRow.Props(
              row,
              columns,
              editor,
              pubidFormat,
              cmdInvoker,
              p.cmdAsync.filterHolistic(cmd => row.actions.exists(_.cmdOption.exists(_ ==* cmd))), // for better Reusability
              issueCategory = csIssueCategory(rowIdx),
              issueClass    = csIssueClass(rowIdx),
              idBase        = csIds(rowIdx),
              titleBase     = csTitles(rowIdx),
            )

            TableRow.Component.withKey(row.key)(rowProps)
          }

      semantic.Table.celledCompactUnstackable(
        *.table,
        header,
        <.tbody(body))
    }
  }
}
