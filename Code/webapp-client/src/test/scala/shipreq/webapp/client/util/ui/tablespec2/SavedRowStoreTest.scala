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
import SavedRowStore.{Row, SS}
import TestUtil._

object SavedRowStoreTest extends TestSuite {

  // -------------------------------------------------------------------------------------------------------------------
  // Props

  type SSS[K, A, B] = SS[K, AB[A,B], (A,B)]

  case class TestInput[S: Equal, K, A: Equal, B: Equal](t: SavedRowStore[S, K, AB[A, B], (A, B)], s: S,
                                                        f: TestFields2[A, B], a: A, b: B, a2: A, b2: B,
                                                        k: K, k2: K, r1: RowStatus, r2: RowStatus) {

    def setab (k: K): S => S = s => t.setField(k, f.f2 * b )(t.setField(k, f.f1 * a )(s))
    def setab2(k: K): S => S = s => t.setField(k, f.f2 * b2)(t.setField(k, f.f1 * a2)(s))
    def revertBoth(k: K): S => S = s => t.revertField(k, f.f1)(t.revertField(k, f.f2)(s))


    def p1 = Eval.equal("set.remove = revert.sync", this,
      t.set(k, t.getP(k)(s))(t.remove(k)(s)),
      t.setStatus(k, Sync)(revertBoth(k)(s)))

    def p2 = Eval.equal("get.set(v).set = v", this,
      t.getI(k)(setab2(k)(setab(k)(s))),
      (a2, b2))

    def p = p1 ∧ p2
  }

  class StoreProps[S: Equal, K, A: Equal, B: Equal] {
    type I = TestInput[S, K, A, B]

    def testNop(name: String, f: I => S) = Prop.equal[I, S](name, f, _.s)
    def main =
      ( //Prop.equal[I]("set.remove = revert")(i⇒{import i._; t.set(k, t.getP(k)(s))(t.remove(k)(s))}, i⇒{import i._; t.setStatus(k, Sync)(revertBoth(k)(s))})
        Prop.eval[I](_.p)
      ∧ Prop.equal[I]("get.set(v).set = v") (i⇒{import i._; t.getI(k)(setab2(k)(setab(k)(s)))}, _.tmap2(_.a2, _.b2))
      ∧ Prop.equal[I]("revertField 1")      (i⇒{import i._; t.getI(k)(t.revertField(k, f.f1)(setab(k)(s)))}, i⇒{import i._; (t.getP(k)(s).a, b)})
      ∧ Prop.equal[I]("revertField 2")      (i⇒{import i._; t.getI(k)(t.revertField(k, f.f2)(setab(k)(s)))}, i⇒{import i._; (a, t.getP(k)(s).b)})
      ) rename "SavedStore"
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Generation

  case class FakeS(i: Int, ss: SSS[Long, Int, String])
  implicit val eqFakeS = Equal.equalA[FakeS]
  val l = SimpleLens[FakeS](_.ss)((a,b) => a.copy(ss = b))

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
      val t = SavedRowStore.of(f).keyedBy[Long].contramap(l)
      TestInput(t, s, f, a1, b1, a2, b2, k1, k2, r1, r2)
    }

  def p = new StoreProps[FakeS, Long, Int, String].main

  override def tests = TestSuite {
    g mustSatisfy p
  }
}