package shipreq.webapp.client.project.app.issues

import japgolly.scalajs.react.test._
import shipreq.webapp.base.test.TestState._

object IssuesPageTestDsl {

  val * = Dsl[Unit, IssuesPageObs, Unit]

  object OptionalEditorDsl extends OptionalEditorDslBase(*, editorCount)

  val rowCount = *.focus("Row count").value(_.obs.rowCount)

  val issueCategories = *.focus("Issue categories").collection(_.obs.columnTexts(Column.IssueCategory))

  val issueClasses = *.focus("Issue classes").collection(_.obs.columnTexts(Column.IssueClass))

  val ids = *.focus("IDs").collection(_.obs.columnTexts(Column.Id))

  val filterValue = *.focus("Filter value").value(_.obs.filter.value)

  val editorCount =
    *.focus("Editor count").value(_.obs.editables.length)

  val invariants: *.Invariants = {

    val summaryTotalMatchesTable =
      *.focus("Row count in summary")
        .value(_.obs.summary.totalIssues)
        .assert.equalBy(x => Some(x.obs.rowCount))

    summaryTotalMatchesTable
  }

  def row(r: Int) = new RowDsl(r)
  final class RowDsl(r: Int) {
    def col(c: Column): CellDsl = new CellDsl(_(r)(c), s"($r,$c)")
    def col(c: Int)   : CellDsl = new CellDsl(_(r)(c), s"($r,$c)")
  }

  final class CellDsl(f: IssuesPageObs => IssuesPageObs.Cell, label: => String)
      extends OptionalEditorDsl.ForCell(f(_).editor, label) {

    def text =
      *.focus(label + " text").value(x => f(x.obs).text)

    def clickAction: *.Actions =
      *.action(NameFn {
        case None    => "Click action"
        case Some(x) => s"Click action: ${f(x.obs).actions.headOption.fold("?")(_.label)}"
      })(x => Simulate click f(x.obs).actions.head.dom())
  }

  object newForm extends OptionalEditorDsl.ForFormWithButton(_.newForm.editor, "new manual issue", _.newForm.button())

  def setFilter(v: String): *.Actions =
    *.action("Set filter to: " + v)(SimEvent.Change(v) simulate _.obs.filter.dom())
      .addCheck(filterValue.assert.equal(v).after)
}
