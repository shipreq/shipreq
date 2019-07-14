package shipreq.webapp.client.project.app.issues

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import shipreq.base.util.ConsolidatedSeq
import shipreq.webapp.base.data._
import shipreq.webapp.base.sort.FusedSorters
import shipreq.webapp.base.ui.semantic
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.feature.RenderFeature
import shipreq.webapp.client.project.widgets.ProjectWidgets

object Table {

  final case class StaticProps(pxProject       : Px[Project],
                               pxRenderFeature : Px[RenderFeature.NoCtx.ForProject],
                               pxProjectWidgets: Px[ProjectWidgets.NoCtx],
                               pxFieldNameFn   : Px[FieldId ~=> String]) {

    val pxPubidFormat = pxProjectWidgets.map(_.PubidFormat(Plain, _ => *.pubidColumnValue, titleFn = _ => None))
    val pxRenderPrep  = Px.apply2(pxProject, pxRenderFeature)(new RenderPrep(_, _))

    val component = ScalaComponent.builder[Props]("Table")
      .backend(new Backend(this, _))
      .renderBackend
      .configure(shouldComponentUpdate)
      .build
  }

  final case class Props()

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
    Sorter.idSorter
    // TODO LooseIssueText | FieldDesc
  )

  final class RenderPrep(p: Project, rf: RenderFeature.NoCtx.ForProject) {
    private val sortFn  = sorter.result(new Sorter.Setup(p))
    private val toRow   = Row.fromIssue(p, rf)
    val rows            = sortFn(p.issues.vector.iterator.map(toRow)).iterator.toVector
    val csIssueCategory = TableRow.consolidateIssueCategories(rows.iterator.map(_.issueCategoryDesc))
    val csIssueClass    = TableRow.consolidateIssueClasses   (rows.iterator.map(_.issueClassDesc))

    private def groupedRows[A, B](groups: ConsolidatedSeq[Any], f: Row => A)(g: (Int, A) => B): Iterator[B] =
      rows.indices.iterator.map { i =>
        val group = groups.group(i)
        val value = f(rows(i))
        g(group, value)
      }

    val csIds = TableRow.Id.consolidate(groupedRows(csIssueClass, {
      case i: Row.ForReq    => Some(\/-(i.req.id))
      case i: Row.ForRcg    => Some(-\/(i.code))
      case _: Row.ForConfig => None
    })(TableRow.Id.apply))

    val csTitles = TableRow.consolidateTitle(groupedRows(csIds, {
      case i: Row.ForReq    => i.req.title
      case i: Row.ForRcg    => i.rcg.title
      case _: Row.ForConfig => Vector.empty
    })((_, _)))
  }

  final class Backend(static: StaticProps, $: BackendScope[Props, Unit]) {
    import static.{component => _, _}

    def render(p: Props): VdomElement = {
      val fieldNames = pxFieldNameFn.value()
      val pubidFormat = pxPubidFormat.value()
      val rp = pxRenderPrep.value()
      import rp._

      val header = TableHeader.Props(columns, fieldNames).render

      val body = rows.indices.toVdomArray { rowIdx =>

        val row = rows(rowIdx)

        val rowProps = TableRow.Props(
          row,
          columns,
          pubidFormat,
          issueCategory = csIssueCategory(rowIdx),
          issueClass    = csIssueClass(rowIdx),
          idBase        = csIds(rowIdx),
          titleBase     = csTitles(rowIdx),
        )

        val key = rowIdx // TODO choose better row key
        TableRow.Component.withKey(key)(rowProps)
      }

      semantic.Table.celledCompactUnstackable(
        *.table,
        header,
        <.tbody(body))
    }
  }
}
