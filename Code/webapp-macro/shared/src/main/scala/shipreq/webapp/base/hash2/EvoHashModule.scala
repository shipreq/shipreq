package shipreq.webapp.base.hash2

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import shipreq.base.util.EqualsByRef

abstract class EvoHashModule[_Scope: UnivEq, _Data] extends EvoHashModule.Types[_Scope, _Data] {

  protected val schemeRegistry: Schemes

  protected def initSchemes(value1: (Scope, HashFn[Data]), values: (Scope, HashFn[Data])*): Schemes =
    Schemes.init(value1, values: _*)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object EvoHashModule {

  class Types[_Scope: UnivEq, _Data] {

    final type Scope = _Scope
    final type Data = _Data

    final type VersionedHashFn = EvoHashModule.VersionedHashFn[Data]
    final val  VersionedHashFn = EvoHashModule.VersionedHashFn

    final type Scheme = EvoHashModule.Scheme[Scope, Data]
    final val  Scheme = EvoHashModule.Scheme

    final type Schemes = EvoHashModule.Schemes[Scope, Data]
    final val  Schemes = EvoHashModule.Schemes

    final type ScopeMap[+A] = EvoHashModule.ScopeMap[Scope, A]

    final type HashRecs = EvoHashModule.HashRecs[Scope, Data]
    object HashRecs {
      def empty: HashRecs = UnivEq.emptyMap
    }

    final type EvolutionOp = EvoHashModule.Schemes.EvolutionOp[Scope, Data]
    final val  EvolutionOp = EvoHashModule.Schemes.EvolutionOp

    final type HashDiscrepancy = EvoHashModule.HashDiscrepancy[Scope, Data]
    final val  HashDiscrepancy = EvoHashModule.HashDiscrepancy

    //  final type ScopeVer = EvoHashModule.ScopeVer
    //  final val  ScopeVer = EvoHashModule.ScopeVer
    //
    //  final type SchemeId = EvoHashModule.SchemeId
    //  final val  SchemeId = EvoHashModule.SchemeId
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class SchemeId(index: Int) extends AnyVal {
    def asChar: Char =
      ('a' + index).toChar
  }

  object SchemeId {
    implicit def univEq: UnivEq[SchemeId] =
      UnivEq.derive

    def fromChar(char: Char): SchemeId =
      apply(char - 'a')
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class ScopeVer(value: Int) extends AnyVal {
    @inline def inc: ScopeVer =
      ScopeVer(value + 1)

    @inline def <=(x: ScopeVer): Boolean =
      value <= x.value
  }

  object ScopeVer {
    val init: ScopeVer =
      apply(1)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class VersionedHashFn[A](ver: ScopeVer, hashFn: HashFn[A]) {
    def addEvolution(hashFn: HashFn[A]): VersionedHashFn[A] =
      VersionedHashFn(ver.inc, hashFn)
  }

  object VersionedHashFn {
    def init[A](hashFn: HashFn[A]): VersionedHashFn[A] =
      apply(ScopeVer.init, hashFn)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  // TODO Optimise & specialise A
  type ScopeMap[S, +A] = Map[S, A]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  type HashRecs[Scope, Data] = Map[Scheme[Scope, Data], ScopeMap[Scope, Option[Int]]]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class Scheme[Scope, Data](id: SchemeId, hashFns: ScopeMap[Scope, VersionedHashFn[Data]]) extends EqualsByRef {

    override def toString = s"HashScope(${id.asChar})"

    def hash(a: Data): ScopeMap[Scope, Int] =
      hashFns.mapValuesNow(_.hashFn(a))
  }

  object Scheme {
    def withoutId[Scope, Data](hashFns: ScopeMap[Scope, VersionedHashFn[Data]]): SchemeId => Scheme[Scope, Data] =
      apply(_, hashFns)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Schemes[Scope, Data](schemesWithoutIds: NonEmptyVector[SchemeId => Scheme[Scope, Data]]) {

    val schemes: NonEmptyVector[Scheme[Scope, Data]] =
      schemesWithoutIds.mapWithIndex((f, i) => f(SchemeId(i)))

    val latest: Scheme[Scope, Data] =
      schemes.last

    val latestId: SchemeId =
      latest.id

    private[this] val allWhole = schemes.whole

    def unsafeGet(id: SchemeId): Scheme[Scope, Data] =
      allWhole(id.index)

    type EvolutionOp = Schemes.EvolutionOp[Scope, Data]

    def addEvolution(op1: EvolutionOp, opN: EvolutionOp*): Schemes[Scope, Data] = {
      import Schemes.EvolutionOp

      val newScopes: ScopeMap[Scope, VersionedHashFn[Data]] =
        (op1 +: opN).foldLeft(latest.hashFns) { (cur, op) =>

          def assertScopeExists(s: Scope) = {
            assert(cur.contains(s), s"Evolution error! Scheme doesn't contain scope: $s")
            cur(s)
          }

          def assertScopeDoesntExist(s: Scope): Unit =
            assert(!cur.contains(s), s"Evolution error! Scheme already contains scope: $s")

          op match {
            case EvolutionOp.Add((s, h)) =>
              assertScopeDoesntExist(s)
              cur.updated(s, VersionedHashFn.init(h))

            case EvolutionOp.Evolve((s, h)) =>
              val old = assertScopeExists(s)
              cur.updated(s, old.addEvolution(h))

            case EvolutionOp.Drop(s) =>
              assertScopeExists(s)
              cur - s
          }
        }

      new Schemes(schemesWithoutIds :+ Scheme.withoutId(newScopes))
    }
  }

  object Schemes {
    sealed trait EvolutionOp[+Scope, +Data] extends Product with Serializable
    object EvolutionOp {
      case class Add   [Scope, Data](kv: (Scope, HashFn[Data])) extends EvolutionOp[Scope, Data]
      case class Evolve[Scope, Data](kv: (Scope, HashFn[Data])) extends EvolutionOp[Scope, Data]
      case class Drop  [Scope]      (k: Scope)                  extends EvolutionOp[Scope, Nothing]
    }

    def init[Scope, Data](value1: (Scope, HashFn[Data]), values: (Scope, HashFn[Data])*): Schemes[Scope, Data] =
      one(Scheme.withoutId((value1 +: values).toMap.mapValuesNow(VersionedHashFn.init)))

    def one[Scope, Data](f: SchemeId => Scheme[Scope, Data]): Schemes[Scope, Data] =
      new Schemes(NonEmptyVector one f)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class HashDiscrepancy[Scope, Data](scheme: Scheme[Scope, Data],
                                                scope : Scope,
                                                expect: Int,
                                                actual: Int) {
    assert(actual != expect)

    def scopeVer: ScopeVer =
      scheme.hashFns(scope).ver

    def msg = s"$scheme.$scope(v$scopeVer) $actual should be $expect."
  }

  object HashDiscrepancy {
    def cmp[Scope, Data](scheme: Scheme[Scope, Data],
                         scope : Scope,
                         actual: Int,
                         expect: Int): Option[HashDiscrepancy[Scope, Data]] =
      Option.when(expect !=* actual)(
        HashDiscrepancy(scheme, scope, expect = expect, actual = actual))
  }

}