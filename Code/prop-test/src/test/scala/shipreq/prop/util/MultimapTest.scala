package shipreq.prop.util

import utest._
import shipreq.prop._
import shipreq.prop.test._
import shipreq.prop.test.TestUtil._
import Multimap.Internal._

object MultimapTest extends TestSuite {

  val gen: Gen[PropInputs[Set, Int]] = {
    val gena = Gen.int
    for {
      kvs ← Gen.tuple2(gena, gena.set).list
      mm  = Multimap(kvs.toMap)
      a   ← gena
      b   ← gena
      as  ← gena.set
    } yield PropInputs[Set, Int](mm, a, b, as)
  }

  case class PropInputs[L[_]: MultiValues, A](mm: Multimap[A, L, A], a: A, b: A, as: L[A]) {
    lazy val aset = as.set
    val em = Multimap.empty[A, L, A]
    val el = implicitly[MultiValues[L]].empty[A]
    def l(x: A): L[A] = el add1 x
    lazy val ab = l(a) add1 b
    lazy val bigas = as add1 a add1 b
    lazy val bigm = mm.addks(bigas, a).addvs(b, bigas)
  }

  def bprop[L[_] : MultiValues, A]: Prop[PropInputs[L, A]] = {
    type MM = Multimap[A, L, A]
    type I = PropInputs[L, A]
    def comm[R](z: MM, r: MM => R, f: MM ⇒ MM, g: MM ⇒ MM) = r(f(g(z))) == r(g(f(z)))
    ( Prop[I]("reverse.reverse = id"      , i⇒{import i._; mm.reverse.reverse           == mm })
    ∧ Prop[I]("addvs symmetrical to addks", i⇒{import i._; mm.addvs(a, as).reverse      == mm.reverse.addks(as, a) })
    ∧ Prop[I]("delvs symmetrical to delks", i⇒{import i._; bigm.delvs(ab).reverse       == bigm.reverse.delks(ab) })
    ∧ Prop[I]("delv symmetrical to delk",   i⇒{import i._; bigm.delv(b).reverse         == bigm.reverse.delk(b) })
    ∧ Prop[I]("get.setvs.add = vs",         i⇒{import i._; em.add(a, b).setvs(a, as)(a) == as })
    ∧ Prop[I]("get.delk.add = ∅",           i⇒{import i._; em.add(a, b).delk(a)(a)      == el })
    ∧ Prop[I]("get.add.delk = v",           i⇒{import i._; em.delk(a).add(a, b)(a)      == l(b) })
    ∧ Prop[I]("get.delk.addvs = ∅",         i⇒{import i._; em.addvs(a, as).delk(a)(a)   == el })
    ∧ Prop[I]("get.addvs.delk = vs",        i⇒{import i._; em.delk(a).addvs(a, as)(a)   == as })
    ∧ Prop[I]("get₁.delk₂.addvs₁ = vs",     i⇒{import i._; em.addvs(a, as).delk(b)(a)   == as })
    ∧ Prop[I]("delk.delk = delks",          i⇒{import i._; bigm.delk(a).delk(b)         == bigm.delks(ab) })
    ∧ Prop[I]("delkv == delv.delk",         i⇒{import i._; bigm.delk(a).delv(a)         == bigm.delkv(a) })
    ∧ Prop[I]("unlink == del(kv).del(vk)",  i⇒{import i._; bigm.del(a, b).del(b, a)     == bigm.unlink(a, b) })
    ∧ Prop[I]("get⁻¹.setks.add = ks",       i⇒{import i._; em.add(b, a).setks(as, a).reverse(a) == as })
    ∧ Prop[I]("addvs.add ⊇⊆ add.addvs",     i⇒{import i._; comm(em, _(a).set, _.addvs(a, as), _.add(a, b)) })
    ∧ Prop[I]("addvs₁₂ ⊇⊆ addvs₂₁",         i⇒{import i._; comm(em, _(a).set, _.addvs(a, as), _.addvs(a, l(b))) })
    ) rename "Multimap"
  }

  val prop = bprop[Set, Int]

  override def tests = TestSuite {
    gen mustSatisfy prop
  }
}