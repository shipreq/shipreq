package shipreq.webapp.client.project.app.issues

import japgolly.microlibs.testutil.TestUtilInternals.quoteStringForDisplay
import japgolly.scalajs.react.test._
import shipreq.webapp.base.test.TestState._

object IssuesPageTestDsl {

  val * = Dsl[Unit, IssuesPageObs, Unit]

  val rowCount = *.focus("Row count").value(_.obs.rowCount)

  val issueCategories = *.focus("Issue categories").collection(_.obs.columnTexts(0))

  val issueClasses = *.focus("Issue classes").collection(_.obs.columnTexts(1))

  val ids = *.focus("IDs").collection(_.obs.columnTexts(2))

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

  final class CellDsl(f: IssuesPageObs => IssuesPageObs.Cell, label: => String) {

    def editorValue =
      *.focus(label + " editor value").value(x => f(x.obs).editorValue)

    def doubleClick: *.Actions =
      *.action(s"Double-click $label text")(x => Simulate doubleClick f(x.obs).td())

    def edit(newValue: String): *.Actions =
      edit(newValue, 1)

    def edit(expectedAndNewValue: (String, String)): *.Actions =
      edit(expectedAndNewValue, 1)

    def edit(newValue: String, editors: Int): *.Actions =
      _editCell(None, newValue, editors)

    def edit(expectedAndNewValue: (String, String), editors: Int): *.Actions =
      _editCell(Some(expectedAndNewValue._1), expectedAndNewValue._2, editors)

    private def _editCell(old: Option[String], newValue: String, editors: Int): *.Actions =
      (doubleClick
        +> editorCount.assert.increaseBy(editors)
        +> editorValue.rename("Initial editor value").assert(old.getOrElse("")).when(_ => old.isDefined) // TODO test-state should support optional assertions
        >> setEditValue(newValue)
        >> commitEdit
        +> editorCount.assert.decreaseBy(editors)
      ).group(s"Edit $label text to ${quoteStringForDisplay(newValue)}")

    def setEditValue(newValue: String): *.Actions =
      *.action(s"Set $label text to ${quoteStringForDisplay(newValue)}")(x =>
        SimEvent.Change(newValue) simulate f(x.obs).editor())

    def commitEdit: *.Actions =
      *.action(s"Commit $label text edit")(x => KB.Enter.ctrl simulateKeyDown f(x.obs).editor())
  }

}
