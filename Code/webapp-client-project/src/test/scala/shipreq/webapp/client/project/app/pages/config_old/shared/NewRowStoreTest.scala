package shipreq.webapp.client.project.app.pages.config_old.shared

import monocle.Lens
import scalaz.Equal
import utest._
import nyaya.prop._
import nyaya.gen._
import nyaya.test._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.client.project.test.ClientTestSettings._
import shipreq.webapp.client.project.test.TestUtil._
import RowStatus._
import NewRowStore.{Row, SS}

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

    def empty = "Empty" rename_: (
      E.test ("enableEdit → active", t.editing(t.enableEdit(s))) ∧
      testNop("remove = id",         t.remove(s))                ∧
      testNop("setStatus = id",      t.setStatus(Sync)(s))       ∧
      E.test ("get = None",          t.get(s).isEmpty)           )

    def active = "Active" rename_: (
      E.test ("remove → empty",       !t.editing(t.remove(s)))                                         ∧
      testNop("enableEdit = id",      t.enableEdit(s))                                                 ∧
      E.equal("get.set(v).set = v",   t.getI(setab2(setab(s))),                         (a2, b2).some) ∧
      E.equal("get.set(rs).set = rs", t.getStatus(t.setStatus(r2)(t.setStatus(r1)(s))), r2.some)       )

    def isActive =
      E.test("is active", t.editing(s))

    def common =
      E.equal("get <=> getI", t.get(s).isEmpty, t.getI(s).isEmpty)

    def main =
      (common ∧ isActive.ifelse(active, empty)) rename "NewStore"
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Generation

  case class FakeS(i: Int, ss: SS[(Int, String)])
  implicit val eqFakeS = Equal.equalA[FakeS]
  val l = Lens((_: FakeS).ss)(b => _.copy(ss = b))

  def genA   = Gen.int
  def genB   = Gen.alphaNumeric.string(1 to 4)
  def genRow = Gen.lift2(genRowStatus, genA *** genB)(Row.apply)
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

  override def tests = Tests {
    g mustSatisfyE (_.main)
  }
}
