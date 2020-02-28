package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq
import shipreq.webapp.base.data.{FilterDead, HideDead, ShowDead}
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.client.project.widgets.SummaryUI
import shipreq.webapp.client.project.widgets.SummaryUI.SummaryIcon

/**
  * Presents a user with a summary of select portions of the content and state of [[ReqTablePage]].
  */
object PageSummary {

  final case class Props(table       : TableContentStats,
                         selectedRows: Int,
                         filterDead  : FilterDead) {
    @inline def render = Component(this)

    def tableFD: TableContentStats =
      filterDead match {
        case HideDead => table.clearDead
        case ShowDead => table
      }
  }

  object Props {
    implicit def equality: UnivEq[Props] =
      UnivEq.derive

    implicit val reusability: Reusability[Props] =
      Reusability.byRefOrUnivEq
  }

  private val iconReappearances = SummaryIcon.reappearances("reappearances due to sorting")

  def summariseContent(table: TableContentStats): TagMod = {
    import table._

    val reqBreakdown: Option[TagMod] = {
      val b = new SummaryUI
      b.beginning = false
      b.addUnlessZero(reqsInProject.dead, SummaryIcon.delete)
      b.addUnlessZero(-reqsFilteredOut.all, SummaryIcon.filter)
      b.resultNonEmpty.map(m => TagMod(" (", reqsInProject.live, m, ")"))
    }

    val rowBreakdown = new SummaryUI
    rowBreakdown.add(uniqueReqsInTable.all, SummaryIcon.reqs, reqBreakdown.whenDefined)
    rowBreakdown.addUnlessZero(reappearances, iconReappearances)
    rowBreakdown.addUnlessZero(codeGroups, SummaryIcon.rcgs)

    rowBreakdown.prefixWithShowing(totalRowsInTable, "row")
  }

  def render(p: Props): VdomElement =
    <.div(
      summariseContent(p.tableFD),
      <.br,
      SummaryUI.selected(p.selectedRows) getOrElse SummaryUI.fakeLine)

  val Component = ScalaComponent.builder[Props]("PageSummary")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}
