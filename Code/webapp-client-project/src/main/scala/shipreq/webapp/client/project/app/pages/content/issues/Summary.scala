package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.client.project.widgets.SummaryUI
import shipreq.webapp.client.project.widgets.SummaryUI.SummaryIcon
import shipreq.webapp.member.project.issue.IssueStats
import shipreq.webapp.member.project.util.DataReusability._

object Summary {

  final case class Props(stats: IssueStats, filteredOut: Int) {
    @inline def render: VdomElement = Component(this)
  }

  object Props {
    implicit def equality: UnivEq[Props] =
      UnivEq.derive

    implicit val reusability: Reusability[Props] =
      Reusability.byRefOrUnivEq
  }

  private val iconReappearances = SummaryIcon.reappearances("having multiple issues")

  def render(p: Props): VdomElement = {
    val s = p.stats
    val b = new SummaryUI

    if (p.filteredOut > 0) {
      val realTotal = p.filteredOut + s.total
      b.addUnlessZero(realTotal, SummaryIcon.issue)
      b.addUnlessZero(-p.filteredOut, SummaryIcon.filter)
      if (s.total > 0) {
        b.add(" = ")
        b.beginning = true
      }
    }

    b.addUnlessZero(s.inConfig, SummaryIcon.config)
    b.addUnlessZero(s.inReq, SummaryIcon.reqs)
    if (s.reqReappearances > 0) {
      b.add(" (", s.reqsUnique)
      b.addUnlessZero(s.reqReappearances, iconReappearances)
      b.add(")")
    }
    b.addUnlessZero(s.inRcg, SummaryIcon.rcgs)
    b.addUnlessZero(s.manual, SummaryIcon.loose)

    <.div(b.prefixWithShowing(s.total, "issue"))
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}
