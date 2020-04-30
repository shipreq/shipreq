package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react.Reusability
import japgolly.univeq.UnivEq
import nyaya.prop._
import shipreq.base.util.fp.Monoid.Implicits._
import shipreq.webapp.base.data.derivation.LiveDeadStat
import shipreq.webapp.base.lib.DataReusability._

/** Stats that describe the contents of [[Table]]. */
final case class TableContentStats(uniqueReqsInTable: LiveDeadStat[Int],
                                   reqsFilteredOut  : LiveDeadStat[Int],
                                   expandedReqs     : Int,
                                   expansionRows    : Int,
                                   codeGroups       : Int) {

  def clearDead: TableContentStats =
    copy(
      uniqueReqsInTable = uniqueReqsInTable.clearDead,
      reqsFilteredOut = reqsFilteredOut.clearDead)

  val reqsInProject: LiveDeadStat[Int] =
    uniqueReqsInTable + reqsFilteredOut

  val reappearances: Int =
    expansionRows - expandedReqs

  val totalRowsInTable: Int =
    uniqueReqsInTable.all + reappearances + codeGroups

  TableContentStats.props.assert(this)
}

object TableContentStats {

  implicit def equality: UnivEq[TableContentStats] =
    UnivEq.derive

  implicit val reusability: Reusability[TableContentStats] =
    Reusability.byRefOrUnivEq

  def props: Prop[TableContentStats] = {
    def positiveI(name: String, f: TableContentStats => Int) =
      Prop.test[TableContentStats](s"$name must be positive", f(_) >= 0)
    def positiveS(name: String, f: TableContentStats => LiveDeadStat[Int]) =
      positiveI(name + ".live", f(_).live) ∧ positiveI(name + ".dead", f(_).dead)

    ( positiveS("uniqueReqsInTable", _.uniqueReqsInTable)
    ∧ positiveS("reqsFilteredOut"  , _.reqsFilteredOut)
    ∧ positiveI("expandedReqs"     , _.expandedReqs)
    ∧ positiveI("expansionRows"    , _.expansionRows)
    ∧ positiveI("codeGroups"       , _.codeGroups)
    ∧ positiveI("reappearances"    , _.reappearances)
    ∧ Prop.test("expandedReqs ≤ uniqueReqsInTable", s => s.expandedReqs <= s.uniqueReqsInTable.all)
    )
  }
}
