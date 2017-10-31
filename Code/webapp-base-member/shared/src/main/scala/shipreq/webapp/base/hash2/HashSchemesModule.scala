package shipreq.webapp.base.hash2

import japgolly.univeq.UnivEq

abstract class HashSchemesModule[@specialized(Char) SchemeId: UnivEq, Scope: UnivEq, Data] {

  protected def schemeIdInc(i: SchemeId): SchemeId

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case class HashCmpFailure(actual: Int, expected: Int) {
    assert(actual != expected)
  }

  case class Scheme(hashFns: Map[Scope, HashFn[Data]]) {

    // dataBefore: Option[A] should be "'hashes with this Scheme' before" for cache-ability
//    def check(dataBefore: Option[A], dataNow: A, recs: Map[S, Option[Int]]): Map[S, HashFailure] =
//      // warn about irrelavent scopes with values
//      // compare hashes for each relevant scope
//      // Hash=None == force pass
//      // missing entry for scope means no change to scope from before
//      ???
  }

  sealed trait EvolutionOp
  object EvolutionOp {
    case class Add   (kv: (Scope, HashFn[Data])) extends EvolutionOp
    case class Evolve(kv: (Scope, HashFn[Data])) extends EvolutionOp
    case class Drop  (k: Scope)                  extends EvolutionOp
  }

  class Schemes(val schemes: Map[SchemeId, Scheme], val latestId: SchemeId) {
    assert(schemes.contains(latestId), s"Latest scheme ($latestId) doesn't exist.")

    val latest = schemes(latestId)

    def addEvolution(op1: EvolutionOp, opN: EvolutionOp*): Schemes = {

      val newScheme: Scheme =
        (op1 +: opN).foldLeft(latest) { (cur, op) =>
          def assertScopeExists(s: Scope) = assert(cur.hashFns.contains(s), s"Latest scheme doesn't contain scope: $s")
          def assertScopeDoesntExist(s: Scope) = assert(!cur.hashFns.contains(s), s"Latest scheme already contains scope: $s")
          op match {

            case EvolutionOp.Add((s, h)) =>
              assertScopeDoesntExist(s)
              Scheme(cur.hashFns.updated(s, h))

            case EvolutionOp.Evolve((s, h)) =>
              assertScopeExists(s)
              Scheme(cur.hashFns.updated(s, h))

            case EvolutionOp.Drop(s) =>
              assertScopeExists(s)
              Scheme(cur.hashFns - s)
          }
        }

      val newId = schemeIdInc(latestId)
      assert(!schemes.contains(newId))
      val newSchemes = schemes.updated(newId, newScheme)
      new Schemes(newSchemes, newId)
    }
  }

  object Schemes {
    def apply(initialId: SchemeId)(initialValue: Scheme): Schemes =
      new Schemes(UnivEq.emptyMap[SchemeId, Scheme].updated(initialId, initialValue), initialId)
  }


}