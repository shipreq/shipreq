package shipreq.webapp.client.app.reqtable

import nyaya.prop._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.SafeStringOps._
import shipreq.webapp.base.UiText.EnglishIntExt
import shipreq.webapp.client.data.{HideDead, FilterDead, ShowDead}

object TableStats {

  def props: Prop[TableStats] = {
    def positive(name: String, f: TableStats => Int) =
      Prop.test[TableStats](s"$name must be positive", f(_) >= 0)

    ( positive("liveVisibleReqs",  _.liveVisibleReqs)
    ∧ positive("deadVisibleReqs",  _.deadVisibleReqs)
    ∧ positive("liveFilteredReqs", _.liveFilteredReqs)
    ∧ positive("deadFilteredReqs", _.deadFilteredReqs)
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
                      deadVisibleReqs : Int,
                      liveFilteredReqs: Int,
                      deadFilteredReqs: Int,
                      expandedReqs    : Int,
                      expansionRows   : Int,
                      codeGroups      : Int) {

  val reappearances = expansionRows - expandedReqs
  val liveReqs      = liveVisibleReqs + liveFilteredReqs
  val deadReqs      = deadVisibleReqs + deadFilteredReqs
  val totalReqs     = liveReqs + deadReqs
  val visibleReqs   = liveVisibleReqs + deadVisibleReqs
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

    def minusFiltered(n: Int) =
      if (n != 0)
        s ~= s" - $n filtered"

    filterDead match {

      case HideDead =>
        val filtered = liveFilteredReqs
        val filterUsed = filtered != 0
        if (filterUsed)
          bracket {
            s ~= s"$liveReqs live"
            minusFiltered(filtered)
          }

      case ShowDead =>
        val filtered = liveFilteredReqs + deadFilteredReqs
        val filterUsed = filtered != 0
        if (liveReqs != 0 && deadReqs == 0 && !filterUsed)
          bracket {
            s ~= "0 deleted"
          }
        else if (liveReqs != 0 || deadReqs != 0 || filterUsed)
          bracket {
            s ~= s"$liveReqs live + $deadReqs deleted"
            minusFiltered(filtered)
          }
    }

    maybeAdd(reappearances, "reappearance")
    maybeAdd(codeGroups, "code group")

    s ~ '.'
  }
}
