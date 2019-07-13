package shipreq.webapp.client.project.app.issues

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import shipreq.webapp.base.data._
import shipreq.webapp.base.sort.FusedSorters
import shipreq.webapp.base.ui.semantic
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

object Table {

  final case class Props(project: Project,
                         pw: ProjectWidgets.AnyCtx,
                         fieldNames: FieldId ~=> String) {
    @inline def render: VdomElement = Component(this)
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.caseClass

  private object Internal {

    val columns: NonEmptyVector[Column] =
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
      // TODO LooseIssueText | Field
    )

    final class RenderPrep(p: Project) {
      private val sortFn  = sorter.result(new Sorter.Setup(p))
      private val toRow   = Row.fromIssue(p)
      val rows            = sortFn(p.issues.vector.iterator.map(toRow)).iterator.toVector
      val csIssueCategory = TableRow.consolidateIssueCategories(rows.iterator.map(_.issueCategoryDesc))
      val csIssueClass    = TableRow.consolidateIssueClasses   (rows.iterator.map(_.issueClassDesc))

      private def iterateByGroup[A, B](f: Row => A)(g: (Int, A) => B): Iterator[B] =
        rows.indices.iterator.map { i =>
          val group = csIssueClass.group(i)
          val value = f(rows(i))
          g(group, value)
        }

      val csIds = TableRow.Id.consolidate(iterateByGroup({
        case i: Row.ForReq    => Some(\/-(i.req.id))
        case i: Row.ForRcg    => Some(-\/(i.code))
        case _: Row.ForConfig => None
      })(TableRow.Id.apply))

      val csTitles = TableRow.consolidateTitle(iterateByGroup({
        case i: Row.ForReq    => i.req.title
        case i: Row.ForRcg    => i.rcg.title
        case _: Row.ForConfig => Vector.empty
      })((_, _)))
    }
  }

  final class Backend($: BackendScope[Props, Unit]) {
    import Internal._

    private val pxProject        = Px.props($).map(_.project).withReuse.autoRefresh
    private val pxProjectWidgets = Px.props($).map(_.pw).withReuse.autoRefresh
    private val pxPubidFormat    = pxProjectWidgets.map(_.PubidFormat(Plain, _ => *.pubidColumnValue, titleFn = _ => None))
    private val pxRenderPrep     = pxProject.map(new RenderPrep(_))

    def render(p: Props): VdomElement = {
      val pubidFormat = pxPubidFormat.value()
      val rp = pxRenderPrep.value()
      import rp._

      val header = TableHeader.Props(columns, p.fieldNames).render

      val body = rows.indices.toVdomArray { rowIdx =>

        val row = rows(rowIdx)

        val rowProps = TableRow.Props(
          row,
          columns,
          p.pw,
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

  val Component = ScalaComponent.builder[Props]("Table")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate)
    .build
}
