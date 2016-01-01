package shipreq.webapp.client.app.reqtable

import utest._
import scalaz.std.string._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.client.data._

object TableStatsTest extends TestSuite {

  def stats(liveVisibleReqs : Int = 0,
            deadVisibleReqs : Int = 0,
            liveFilteredReqs: Int = 0,
            deadFilteredReqs: Int = 0,
            reappearances   : Int = 0,
            codeGroups      : Int = 0)(implicit fd: FilterDead): TableStats =
    TableStats(fd,
      liveVisibleReqs  = liveVisibleReqs ,
      liveFilteredReqs = liveFilteredReqs,
      deadVisibleReqs  = if (fd :: HideDead) 0 else deadVisibleReqs,
      deadFilteredReqs = deadFilteredReqs + (if (fd :: ShowDead) 0 else deadVisibleReqs),
      expandedReqs     = reappearances   ,
      expansionRows    = reappearances*2 ,
      codeGroups       = codeGroups      )

  def test(liveVisibleReqs : Int = 0,
           deadVisibleReqs : Int = 0,
           liveFilteredReqs: Int = 0,
           deadFilteredReqs: Int = 0,
           reappearances   : Int = 0,
           codeGroups      : Int = 0)
          (whenHideDead    : String,
           whenShowDead    : String): Unit = {

    def go(fd: FilterDead, exp: String) = {
      val s = stats(
        liveVisibleReqs  = liveVisibleReqs ,
        liveFilteredReqs = liveFilteredReqs,
        deadVisibleReqs  = deadVisibleReqs ,
        deadFilteredReqs = deadFilteredReqs,
        reappearances    = reappearances   ,
        codeGroups       = codeGroups      )(fd)
      assertEq(s.toString, s.summary, exp)
    }

    go(HideDead, whenHideDead)
    go(ShowDead, whenShowDead)
  }

  val hideDeadEmpty = "0 rows: 0 reqs."

  override def tests = TestSuite {

    * - test()(
      hideDeadEmpty,
      hideDeadEmpty)

    * - test(liveVisibleReqs = 1)(
      "1 row: 1 req.",
      "1 row: 1 req (0 deleted).")

    * - test(liveVisibleReqs = 8)(
      "8 rows: 8 reqs.",
      "8 rows: 8 reqs (0 deleted).")

    * - test(liveFilteredReqs = 7)(
      "0 rows: 0 reqs (7 live - 7 filtered).",
      "0 rows: 0 reqs (7 live + 0 deleted - 7 filtered).")

    * - test(liveVisibleReqs = 8, reappearances = 2)(
      "10 rows: 8 reqs + 2 reappearances.",
      "10 rows: 8 reqs (0 deleted) + 2 reappearances.")

    * - test(liveVisibleReqs = 8, reappearances = 2, codeGroups = 3)(
      "13 rows: 8 reqs + 2 reappearances + 3 code groups.",
      "13 rows: 8 reqs (0 deleted) + 2 reappearances + 3 code groups.")

    * - test(liveVisibleReqs = 8, codeGroups = 3)(
      "11 rows: 8 reqs + 3 code groups.",
      "11 rows: 8 reqs (0 deleted) + 3 code groups.")

    * - test(liveVisibleReqs = 4, deadVisibleReqs = 2, liveFilteredReqs = 3)(
      "4 rows: 4 reqs (7 live - 3 filtered).",
      "6 rows: 6 reqs (7 live + 2 deleted - 3 filtered).")

    * - test(liveVisibleReqs = 4, deadVisibleReqs = 2, deadFilteredReqs = 3)(
      "4 rows: 4 reqs.",
      "6 rows: 6 reqs (4 live + 5 deleted - 3 filtered).")

    * - test(liveVisibleReqs = 4, deadVisibleReqs = 2, liveFilteredReqs = 3, deadFilteredReqs = 10)(
      "4 rows: 4 reqs (7 live - 3 filtered).",
      "6 rows: 6 reqs (7 live + 12 deleted - 13 filtered).")

    * - test(liveVisibleReqs = 5, deadVisibleReqs = 2)(
      "5 rows: 5 reqs.",
      "7 rows: 7 reqs (5 live + 2 deleted).")

    * - test(liveVisibleReqs = 6, liveFilteredReqs = 3)(
      "6 rows: 6 reqs (9 live - 3 filtered).",
      "6 rows: 6 reqs (9 live + 0 deleted - 3 filtered).")

    * - test(liveVisibleReqs = 9, deadVisibleReqs = 2, liveFilteredReqs = 3, reappearances = 4, codeGroups = 5)(
      "18 rows: 9 reqs (12 live - 3 filtered) + 4 reappearances + 5 code groups.",
      "20 rows: 11 reqs (12 live + 2 deleted - 3 filtered) + 4 reappearances + 5 code groups.")

    * - test(liveVisibleReqs = 1, deadVisibleReqs = 1, liveFilteredReqs = 1, reappearances = 1, codeGroups = 1)(
      "3 rows: 1 req (2 live - 1 filtered) + 1 reappearance + 1 code group.",
      "4 rows: 2 reqs (2 live + 1 deleted - 1 filtered) + 1 reappearance + 1 code group.")

    * - test(deadVisibleReqs = 5, liveFilteredReqs = 2)(
      "0 rows: 0 reqs (2 live - 2 filtered).",
      "5 rows: 5 reqs (2 live + 5 deleted - 2 filtered).")

    * - test(deadVisibleReqs = 7)(
      hideDeadEmpty,
      "7 rows: 7 reqs (0 live + 7 deleted).")

    * - test(deadFilteredReqs = 7)(
      hideDeadEmpty,
      "0 rows: 0 reqs (0 live + 7 deleted - 7 filtered).")

    * - test(deadVisibleReqs = 3, deadFilteredReqs = 2)(
      hideDeadEmpty,
      "3 rows: 3 reqs (0 live + 5 deleted - 2 filtered).")
  }
}
