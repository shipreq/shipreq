package shipreq.webapp.client.lib.ui

import monocle.Lens
import scalaz.Equal
import scalaz.std.AllInstances._
import utest._
import shipreq.prop._
import shipreq.prop.test._
import shipreq.prop.test.PropTest._
import RowStatus._
import SavedRowStore.{Row, SS}
import TestUtil._

object SavedRowStoreTest extends TestSuite {

  // -------------------------------------------------------------------------------------------------------------------
  // Props

  type SSS[K, A, B] = SS[K, AB[A,B], (A,B)]

  case class TestInput[S: Equal, K, A: Equal, B: Equal](t: SavedRowStore[S, K, AB[A, B], (A, B)], s: S,
                                                        f: TestFields2[A, B], a: A, b: B, a2: A, b2: B,
                                                        k: K, k2: K, r1: RowStatus, r2: RowStatus) {
    val E = EvalOver(this)
    def setab (k: K): S => S = s => t.setField(k, f.f2 * b )(t.setField(k, f.f1 * a )(s))
    def setab2(k: K): S => S = s => t.setField(k, f.f2 * b2)(t.setField(k, f.f1 * a2)(s))
    def revertBoth(k: K): S => S = s => t.revertField(k, f.f1)(t.revertField(k, f.f2)(s))

    def eval =
    ( E.equal("set.remove = revert.sync", t.set(k, t.getP(k)(s))(t.remove(k)(s)),         t.setStatus(k, Sync)(revertBoth(k)(s)))
    ∧ E.equal("get.set(v).set = v",       t.getI(k)(setab2(k)(setab(k)(s))),              (a2, b2))
    ∧ E.equal("get.set(v).set = v",       t.getI(k)(setab2(k)(setab(k)(s))),              (a2, b2))
    ∧ E.equal("revertField 1",            t.getI(k)(t.revertField(k, f.f1)(setab(k)(s))), (t.getP(k)(s).a, b))
    ∧ E.equal("revertField 2",            t.getI(k)(t.revertField(k, f.f2)(setab(k)(s))), (a, t.getP(k)(s).b))
    ) rename "SavedStore"
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Generation

  case class FakeS(i: Int, ss: SSS[Long, Int, String])
  implicit val eqFakeS = Equal.equalA[FakeS]
  val l = Lens((_: FakeS).ss)(b => _.copy(ss = b))

  def genA   = Gen.int
  def genB   = Gen.alphanumericstring1.lim(4)
  def genK   = Gen.long
  def genRow = Gen.apply3(Row.apply[AB[Int, String], (Int, String)])(genRowStatus, genAB(genA, genB), Gen.pair(genA, genB))
  def genS   = Gen.apply2(FakeS)(Gen.int, genRow.mapBy(genK))
  def g =
    for {
      (a0,a1,a2) ← genA.triple
      (b0,b1,b2) ← genB.triple
      (k1,k2)    ← genK.pair
      (v1,v2)    ← genRow.pair
      s0         ← genS
      s          = s0.copy(ss = s0.ss + (k1 -> v1) + (k2 -> v2))
      (r1,r2)    ← genRowStatus.pair
    } yield {
      val f = fields2((a0,b0))
      val t = SavedRowStore.fields(f).keyedBy[Long].contramap(l)
      TestInput(t, s, f, a1, b1, a2, b2, k1, k2, r1, r2)
    }

  override def tests = TestSuite {
    g mustSatisfyE (_.eval)
  }
}
