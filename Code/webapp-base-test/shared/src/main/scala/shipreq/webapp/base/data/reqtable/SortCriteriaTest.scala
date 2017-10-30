package shipreq.webapp.base.data.reqtable

import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTest._
import utest._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.test.WebappTestUtil._
import SortCriterion._
import SortMethod._
import Column._

object SortCriteriaTest extends TestSuite {

  /*
  def testWant(before: SortCriteria, want: Column)(expect: SortCriteria): Unit =
    assertEq(s"has $before, want $want", before want want, expect)

  val sc1 = SortCriteria(Vector(Code / AscThenBlanks, ReqType / Desc, Title / BlanksThenAsc), Pubid / Desc)

  override def tests = TestSuite {
    'want {
      'inconclusive {
        'new_cb - testWant(sc1, Tags, )
        'new_ib - testWant(sc1, Tags, )
        'head_cb1234 - ???
        'head_ib12   - ???
        'mid - ???
        'last - ???
      }
      'conclusive {
        'same12 - ???
        'diff - ???
      }
    }
  }
  */

  case class WantTest(a: SortCriteria, i: Column.SortInconclusive, c: Column.SortConclusive) {
    val E = EvalOver(this)

    def primaryHasChanged(b: SortCriteria) = {
      val pa = (a.init :+ a.last).head
      val pb = (b.init :+ b.last).head
      E.equal("", pb, pa).not.rename("primary has changed")
    }

    def wantInconclusive = {
      val b = a want i
      def remove_i(sc: SortCriteria) = sc.filterColumns(_ ≠ i)
      "wantInconclusive" rename_: (
        primaryHasChanged(b) ∧
        E.equal("w is at head", Option(i),   b.init.headOption.map(_.column)) ∧
        E.equal("a-w = b-w",    remove_i(b), remove_i(a)))
    }

    def wantConclusive = {
      val b = a want c
      "wantConclusive" rename_: (
        primaryHasChanged(b) ∧
        E.equal("w is at tail", c,      b.last.column) ∧
        E.equal("clear init",   b.init, Vector.empty))
    }

    def all = wantInconclusive ∧ wantConclusive
  }

  val wantTest = for {
    cfs  ← RandomData.reqtableData.customFieldColumn.vector
    gi   = RandomData.reqtableData.ColumnIGen(cfs)
    scis ← gi.sortCriIs
    ci   ← gi.columnI
    cc   ← RandomData.reqtableData.columnC
    sc   ← RandomData.reqtableData.sortCriteria(scis)
  } yield WantTest(sc, ci, cc)

  override def tests = TestSuite {
    'want {
      'props - wantTest.mustSatisfyE(_.all)
    }
  }
}