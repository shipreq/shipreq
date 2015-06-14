package shipreq.webapp.client.app.ui.reqtable

import japgolly.nyaya._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.SafeStringOps._
import shipreq.webapp.base.UiText.EnglishIntExt
import shipreq.webapp.client.lib.{HideDead, FilterDead, ShowDead}

object TableStats {

  def props: Prop[TableStats] = {
    def positive(name: String, f: TableStats => Int) =
      Prop.test[TableStats](s"$name must be positive", f(_) >= 0)

    ( positive("liveVisibleReqs",  _.liveVisibleReqs)
    ∧ positive("liveFilteredReqs", _.liveFilteredReqs)
    ∧ positive("deadReqs",         _.deadReqs)
    ∧ positive("expandedReqs",     _.expandedReqs)
    ∧ positive("expansionRows",    _.expansionRows)
    ∧ positive("codeGroups",       _.codeGroups)
    ∧ positive("reappearances",    _.reappearances)
    ∧ positive("totalReqs",        _.totalReqs)
    ∧ positive("visibleReqs",      _.visibleReqs)
    ∧ positive("visibleRows",      _.visibleRows)
    ∧ Prop.test("expandedReqs ≤ visibleReqs", s => s.expandedReqs <= s.visibleReqs)
    )
  }

  implicit val reusability: Reusability[TableStats] = Reusability.by(_.summary)
}

case class TableStats(filterDead      : FilterDead,
                      liveVisibleReqs : Int,
                      liveFilteredReqs: Int,
                      deadReqs        : Int,
                      expandedReqs    : Int,
                      expansionRows   : Int,
                      codeGroups      : Int) {

  val reappearances = expansionRows - expandedReqs
  val liveReqs      = liveVisibleReqs + liveFilteredReqs
  val totalReqs     = liveReqs + deadReqs
  val visibleReqs   = liveVisibleReqs + (if (filterDead :: ShowDead) deadReqs else 0)
  val visibleRows   = visibleReqs + reappearances + codeGroups

  this assertSatisfies TableStats.props

  val summary: String = {
    var s = visibleRows.unitsOf("row") ~ ": " ~ visibleReqs.unitsOf("req")

    def bracket(f: => Unit): Unit = {
      s ~= " ("
      f
      s ~= ")"
    }

    def maybeAdd(i: Int, u: String, us: String = null): Unit =
      if (i > 0)
        s ~= " + " ~ i.unitsOf(u, us)

    val filterUsed = liveFilteredReqs != 0

    def minusFiltered() =
      if (filterUsed)
        s ~= s" - $liveFilteredReqs filtered"

    filterDead match {
      case HideDead =>
        if (filterUsed)
          bracket {
            s ~= s"$liveReqs live"
            minusFiltered()
          }
      case ShowDead =>
        if (liveReqs != 0 && deadReqs == 0 && !filterUsed)
          bracket {
            s ~= "0 deleted"
          }
        else if (liveReqs != 0 || deadReqs != 0 || filterUsed)
          bracket {
            s ~= s"$liveReqs live + $deadReqs deleted"
            minusFiltered()
          }
    }

    maybeAdd(reappearances, "reappearance")
    maybeAdd(codeGroups, "code group")

    s ~ '.'
  }
}
