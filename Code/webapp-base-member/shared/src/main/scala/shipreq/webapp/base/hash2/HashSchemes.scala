package shipreq.webapp.base.hash2

import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.webapp.base.data.Project
import HashSchemes.EvolutionOp

final class HashSchemes(schemesWithoutIds: NonEmptyVector[HashSchemeId => HashScheme]) {

  private[hash2] val schemes: NonEmptyVector[HashScheme] =
    schemesWithoutIds.mapWithIndex((f, i) => f(HashSchemeId.zero.plus(i)))

  val latest: HashScheme =
    schemes.last

  val latestId: HashSchemeId =
    HashSchemeId.zero.plus(schemes.length - 1)

  private[this] val allWhole = schemes.whole

  def unsafeGet(id: HashSchemeId): HashScheme =
    allWhole(id.value - HashSchemeId.zero.value)

  private[hash2] def addEvolution(op1: EvolutionOp, opN: EvolutionOp*): HashSchemes = {
    val newScopes: HashScope.VersionedHashFns =
      (op1 +: opN).foldLeft(latest.hashFns) { (cur, op) =>

        def assertScopeExists(s: HashScope) = {
          assert(cur.contains(s), s"Evolution error! Scheme doesn't contain scope: $s")
          cur.need(s)
        }

        def assertScopeDoesntExist(s: HashScope): Unit =
          assert(!cur.contains(s), s"Evolution error! Scheme already contains scope: $s")

        op match {
          case EvolutionOp.Add((s, h)) =>
            assertScopeDoesntExist(s)
            cur.updated(s, HashScope.VersionedHashFn.init(h))

          case EvolutionOp.Evolve((s, h)) =>
            val old = assertScopeExists(s)
            cur.updated(s, old.evolve(h))

          case EvolutionOp.Drop(s) =>
            assertScopeExists(s)
            cur - s
        }
      }

    new HashSchemes(schemesWithoutIds :+ HashScheme.withoutId(newScopes))
  }
}

// ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object HashSchemes {

  private[hash2] sealed trait EvolutionOp

  private[hash2] object EvolutionOp {
    case class Add   (kv: (HashScope, HashFn[Project])) extends EvolutionOp
    case class Evolve(kv: (HashScope, HashFn[Project])) extends EvolutionOp
    case class Drop  (k: HashScope)                     extends EvolutionOp
  }

  private[hash2] def init(values: (HashScope, HashFn[Project])*): HashSchemes =
    initF(HashScheme.withoutId(
      HashScope.To(values.toMap).map(h => HashScope.VersionedHashFn.init(h))))

  private[hash2] def initF(f: HashSchemeId => HashScheme): HashSchemes =
    new HashSchemes(NonEmptyVector one f)

  val Registry: HashSchemes =
    init(
      HashScope.ProjectName     --> ProjectHasher.hashProjectName,
      HashScope.CfgIssueTypes   --> ProjectHasher.hashCustomIssueTypes,
      HashScope.CfgReqTypes     --> ProjectHasher.hashReqTypes,
      HashScope.CfgFields       --> ProjectHasher.hashFieldSet,
      HashScope.CfgTags         --> ProjectHasher.hashTagTree,
      HashScope.GenericReqs     --> ProjectHasher.hashGenericReqs,
      HashScope.UseCases        --> ProjectHasher.hashUseCases,
      HashScope.PubidRegister   --> ProjectHasher.hashPubidRegister,
      HashScope.ReqCodes        --> ProjectHasher.hashReqCodes,
      HashScope.TextFieldData   --> ProjectHasher.hashReqDataText,
      HashScope.TagData         --> ProjectHasher.hashReqDataTags,
      HashScope.ImplicationData --> ProjectHasher.hashImplications,
      HashScope.DeletionReasons --> ProjectHasher.hashDeletionReasons,
      HashScope.SavedViews      --> ProjectHasher.hashSavedViews,
    )

}
