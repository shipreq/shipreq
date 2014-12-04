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

  case class TestInput[S, A, B](t: NewRowStore[S, (A,B)], s: S, f: TestFields2[A, B], a: A, b: B, a2: A, b2: B, r1: RowStatus, r2: RowStatus) {
    def setab : S => S = s => t.setField(f.f2 * b )(t.setField(f.f1 * a )(s))
    def setab2: S => S = s => t.setField(f.f2 * b2)(t.setField(f.f1 * a2)(s))
  }

  class StoreProps[S: Equal, A: Equal, B: Equal] {
    type I = TestInput[S, A, B]

    def testNop(name: String, f: I => S) = Prop.equal[I, S](name, f, _.s)

    def empty =
      ( Prop[I]("enableEdit → active", i⇒{import i._; t.editing(t.enableEdit(s))})
      ∧ testNop("remove = id",         i⇒{import i._; t.remove(s) })
      ∧ testNop("setStatus = id",      i⇒{import i._; t.setStatus(Sync)(s) })
      ∧ Prop[I]("get = None",          i⇒{import i._; t.get(s).isEmpty})
      ) rename "Empty"

    def active =
      ( Prop[I]      ("remove → empty",       i⇒{import i._; !t.editing(t.remove(s))})
      ∧ testNop      ("enableEdit = id",      i⇒{import i._; t.enableEdit(s) })
      ∧ Prop.equal[I]("get.set(v).set = v")  (i⇒{import i._; t.getI(setab2(setab(s)))}, _.tmap2(_.a2, _.b2).some)
      ∧ Prop.equal[I]("get.set(rs).set = rs")(i⇒{import i._; t.getStatus(t.setStatus(r2)(t.setStatus(r1)(s)))}, _.r2.some)
      ) rename "Active"

    def isActive = Prop[I]("is active", i => i.t.editing(i.s))

    def main =
      (Prop[I]("get <=> getI", i⇒{import i._; t.get(s).isEmpty == t.getI(s).isEmpty})
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

  def p = new StoreProps[FakeS, Int, String].main

  override def tests = TestSuite {
    g mustSatisfy p
  }
}