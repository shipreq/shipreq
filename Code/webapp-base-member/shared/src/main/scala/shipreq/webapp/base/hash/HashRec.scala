package shipreq.webapp.base.hash

import scala.collection.immutable.ListSet
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.ApplyEvent.LogicVer
import HashRec.ValidationFailure

/**
 * Hash record.
 *
 * Its equality and hashCode exclude the hash itself.
 *
 * @param hash `None` means "disable integrity checking".
 */
final case class HashRec(scope   : HashScope,
                         logicVer: LogicVer,
                         scheme  : HashScheme)(
                     val hash    : Option[Int]) {

  override def toString =
    s"HashRec($scope, $logicVer, $scheme)(${hash.fold("∅")(_.toString)})"

  def recalc(p: Project): Int =
    scheme.hasher(scope, p)

  def validate(p: Project): Validity =
    Valid when validateF(p).isEmpty

  def validateF(p: Project): Option[ValidationFailure] =
    if (logicVer.isCurrent)
      hash.flatMap { e =>
        val a = recalc(p)
        if (e ==* a)
          None
        else
          Some(ValidationFailure(expect = e, actual = a))
      }
    else
      // Can't validate old logic; new logic is always applied.
      None
}

object HashRec {

  case class ValidationFailure(expect: Int, actual: Int) {
    def msg = s"$actual should be $expect."
  }

  type Collection = ListSet[HashRec]

  val emptyCollection: Collection =
    ListSet.empty

  implicit def equality: UnivEq[HashRec] = UnivEq.derive
  implicit def collectionEquality: UnivEq[Collection] = UnivEq.univEqListSet

  val defaultHashScopes = HashScope.defaultSet.whole

  def apply(p: Project): HashRec.Collection = {
    val scheme = HashScheme.latest
    var r = emptyCollection
    val d = scheme.hasher
    for (s <- defaultHashScopes) {
      val h = d(s, p)
      r += HashRec(s, LogicVer.Current, scheme)(Some(h))
    }
    r
  }

  def changes(p1: Project, p2: Project): HashRec.Collection =
    __changes(defaultHashScopes, LogicVer.Current, HashScheme.latest, p1, p2)

  /** Public for testing */
  def __changes(scopes: TraversableOnce[HashScope], lv: LogicVer, scheme: HashScheme, p1: Project, p2: Project): HashRec.Collection = {
    var r = emptyCollection
    val d = scheme.hasher
    for (s <- scopes) {
      val h1 = d(s, p1)
      val h2 = d(s, p2)
      if (h1 !=* h2)
        r += HashRec(s, lv, scheme)(Some(h2))
    }
    r
  }

  def merge(older: Collection, newer: Collection): Collection =
    // Any existence of old logic completely invalidates past results because there's (currently) no way of determining
    // how the change in logic impacts previously-generated hashes. Therefore, we throw out all old hashes.
    if (older.isEmpty || newer.exists(!_.logicVer.isCurrent))
      newer
    else if (newer.isEmpty)
      older
    else {
      // Add non-overlapping old

      val totalNew = {
        val b = Set.newBuilder[HashScope]
        for (n <- newer) {
          b ++= reflSubsets(n)
          b ++= n.scheme.invalidScopes
        }
        b.result()
      }
      val totalNewContains = totalNew.contains _

      var result = newer
      for (oldRec <- older) {
        val totalOld = reflSubsets(oldRec)
        if (!totalOld.exists(totalNewContains))
          result += oldRec
      }
      result
    }

  val reflSubsets: HashRec => Set[HashScope] = {
    import HashScope._
    type Lookup = HashScope => Set[HashScope]

    def mkReflSubsets(directNonReflSubsets: Lookup): Lookup = {
      val tc = TransitiveClosure.auto(all.whole)(directNonReflSubsets)
      all.iterator.map(s => (s, tc(s))).toMap.apply
    }

    val latest = mkReflSubsets {
      case WholeProject    => Set(Config, Content, Other)
      case Config          => Set(CfgIssueTypes, CfgReqTypes, CfgFields, CfgTags)
      case Content         => Set(Reqs, ReqCodes, TextFieldData, TagData, ImplicationData, DeletionReasons)
      case Reqs            => Set(GenericReqs, UseCases, PubidRegister)
      case CfgIssueTypes
         | CfgReqTypes
         | CfgFields
         | CfgTags
         | GenericReqs
         | UseCases
         | PubidRegister
         | ReqCodes
         | TextFieldData
         | TagData
         | ImplicationData
         | DeletionReasons
         | Other           => Set.empty
    }

    // The original idea was that logicVer and hashScheme would be used to affect the results here.
    // 1) That's problematic in that how can fake hashSchemes be added in tests.
    // 2) The newer HashScheme.invalidScopes already provides some functionality that was previously envisioned to go here.
    r => latest(r.scope)
  }
}
