package shipreq.prop.util

import scalaz.{Order, Equal}
import scalaz.std.anyVal._
import scalaz.std.set._
import utest._
import shipreq.prop._
import shipreq.prop.test._
import shipreq.prop.test.PropTest._
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

  case class PropInputs[L[_] : MultiValues, A](mm: Multimap[A, L, A], a: A, b: A, as: L[A])
                                              (implicit A: Order[A], L: Equal[L[A]]) {
    val E = EvalOver(this)

    type MM = Multimap[A, L, A]
    lazy val aset = as.set
    val em = Multimap.empty[A, L, A]
    val el = MultiValues[L].empty[A]
    def l(x: A): L[A] = el add1 x
    lazy val ab = l(a) add1 b
    lazy val bigas = as add1 a add1 b
    lazy val bigm = mm.addks(bigas, a).addvs(b, bigas)
    def comm[R: Equal](name: => String, z: MM, r: MM => R, f: MM ⇒ MM, g: MM ⇒ MM) =
      E.equal(name, r(f(g(z))), r(g(f(z))))

    def eval =
    ( E.equal("reverse.reverse = id"      , mm.reverse.reverse                  , mm)
    ∧ E.equal("addvs symmetrical to addks", mm.addvs(a, as).reverse             , mm.reverse.addks(as, a))
    ∧ E.equal("delvs symmetrical to delks", bigm.delvs(ab).reverse              , bigm.reverse.delks(ab))
    ∧ E.equal("delv symmetrical to delk",   bigm.delv(b).reverse                , bigm.reverse.delk(b))
    ∧ E.equal("get.setvs.add = vs",         em.add(a, b).setvs(a, as)(a)        , as)
    ∧ E.equal("get.delk.add = ∅",           em.add(a, b).delk(a)(a)             , el)
    ∧ E.equal("get.add.delk = v",           em.delk(a).add(a, b)(a)             , l(b))
    ∧ E.equal("get.delk.addvs = ∅",         em.addvs(a, as).delk(a)(a)          , el)
    ∧ E.equal("get.addvs.delk = vs",        em.delk(a).addvs(a, as)(a)          , as)
    ∧ E.equal("get₁.delk₂.addvs₁ = vs",     em.addvs(a, as).delk(b)(a)          , as)
    ∧ E.equal("delk.delk = delks",          bigm.delk(a).delk(b)                , bigm.delks(ab))
    ∧ E.equal("delkv == delv.delk",         bigm.delk(a).delv(a)                , bigm.delkv(a))
    ∧ E.equal("unlink == del(kv).del(vk)",  bigm.del(a, b).del(b, a)            , bigm.unlink(a, b))
    ∧ E.equal("get⁻¹.setks.add = ks",       em.add(b, a).setks(as, a).reverse(a), as)
    ∧ comm("addvs.add ⊇⊆ add.addvs", em, _(a).set, _.addvs(a, as), _.add(a, b))
    ∧ comm("addvs₁₂ ⊇⊆ addvs₂₁",     em, _(a).set, _.addvs(a, as), _.addvs(a, l(b)))
    ) rename "Multimap"
  }

  override def tests = TestSuite {
    gen mustSatisfyE (_.eval)
  }
}