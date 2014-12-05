package shipreq.webapp.client.util.ui.tablespec2

import monocle.SimpleLens
import scalaz.Equal
import scalaz.std.AllInstances._
import utest._
import shipreq.prop._
import shipreq.prop.test._
import shipreq.prop.test.PropTest._
import shipreq.base.util.ScalaExt._
import RowStatus._
import NewRowStore.{Row, SS}
import TestUtil._

object NewRowStoreTest extends TestSuite {

  // -------------------------------------------------------------------------------------------------------------------
  // Props

  case class TestInput[S: Equal, A: Equal, B: Equal](t: NewRowStore[S, (A,B)], s: S,
                                                     f: TestFields2[A, B], a: A, b: B, a2: A, b2: B,
                                                     r1: RowStatus, r2: RowStatus) {
    val E = EvalOver(this)
    def setab : S => S = s => t.setField(f.f2 * b )(t.setField(f.f1 * a )(s))
    def setab2: S => S = s => t.setField(f.f2 * b2)(t.setField(f.f1 * a2)(s))

    def testNop(name: => String, t: S) = E.equal(name, t, s)

    def empty =
      ( E.test ("enableEdit → active", t.editing(t.enableEdit(s)))
      ∧ testNop("remove = id",         t.remove(s))
      ∧ testNop("setStatus = id",      t.setStatus(Sync)(s))
      ∧ E.test ("get = None",          t.get(s).isEmpty)
      ) rename "Empty"

    def active =
      ( E.test ("remove → empty",       !t.editing(t.remove(s)))
      ∧ testNop("enableEdit = id",      t.enableEdit(s))
      ∧ E.equal("get.set(v).set = v",   t.getI(setab2(setab(s))),                         (a2, b2).some)
      ∧ E.equal("get.set(rs).set = rs", t.getStatus(t.setStatus(r2)(t.setStatus(r1)(s))), r2.some)
      ) rename "Active"

    def isActive = E.test("is active", t.editing(s))

    def main =
      ( E.equal("get <=> getI", t.get(s).isEmpty, t.getI(s).isEmpty)
      ∧ isActive.ifelse(active, empty)
      ) rename "NewStore"
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Generation

  case class FakeS(i: Int, ss: SS[(Int, String)])
  implicit val eqFakeS = Equal.equalA[FakeS]
  val l = SimpleLens[FakeS](_.ss)((a,b) => a.copy(ss = b))

  def genA   = Gen.int
  def genB   = Gen.alphanumericstring1.lim(4)
  def genRow = Gen.apply2(Row.apply[(Int, String)])(genRowStatus, Gen.pair(genA, genB))
  def genS   = Gen.apply2(FakeS)(Gen.int, genRow.option)
  def g      =
    for {
      (a0,a1,a2) <- genA.triple
      (b0,b1,b2) <- genB.triple
      s          <- genS
      (r1,r2)    <- genRowStatus.pair
    } yield {
      val f = fields2((a0,b0))
      val t = NewRowStore.of(f).contramap(l)
      TestInput(t, s, f, a1, b1, a2, b2, r1, r2)
    }

  def p = Prop.eval[TestInput[FakeS, Int, String]](_.main)

  override def tests = TestSuite {
    g mustSatisfy p
  }
}