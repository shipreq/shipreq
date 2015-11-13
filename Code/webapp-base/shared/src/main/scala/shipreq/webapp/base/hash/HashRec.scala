package shipreq.webapp.base.hash

import scala.collection.immutable.ListSet
import scalaz.syntax.equal._
import shipreq.base.util._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.ApplyEvent.LogicVer
import UnivEq.Implicits.univEqInt

/**
 * Hash record.
 *
 * Its equality and hashCode exclude the hash itself.
 *
 * @param hash `None` means "disable integrity checking".
 */
case class HashRec(scope   : HashScope,
                   logicVer: LogicVer,
                   scheme  : HashScheme)(
               val hash    : Option[Int]) {

  def recalc(p: Project): Int =
    HashScope.hash(scope, scheme.value, p)

  def validate(p: Project): Validity =
    hash match {
      case Some(h) => Valid <~ (h ==* recalc(p))
      case None    => Valid
    }
}

object HashRec {
  type Collection = ListSet[HashRec]

  val emptyCollection: Collection =
    ListSet.empty

  implicit def equality: UnivEq[HashRec] = UnivEq.derive
  implicit def collectionEquality: UnivEq[Collection] = UnivEq.univEqListSet

  private val defaultHashScopes = HashScope.defaultSet.whole

  def changes(p1: Project, p2: Project): HashRec.Collection =
    changes(defaultHashScopes, LogicVer.Current, HashScheme.latest, p1, p2)

  private def changes(scopes: TraversableOnce[HashScope], lv: LogicVer, scheme: HashScheme, p1: Project, p2: Project): HashRec.Collection = {
    var r = emptyCollection
    val d = scheme.value
    for (s <- scopes) {
      val h1 = HashScope.hash(s, d, p1)
      val h2 = HashScope.hash(s, d, p2)
      if (h1 !=* h2)
        r += HashRec(s, lv, scheme)(Some(h2))
    }
    r
  }

  def merge(older: Collection, newer: Collection): Collection =
    if (older.isEmpty)
      newer
    else if (newer.isEmpty)
      older
    else {
      // Add non-overlapping old
      val totalNew = newer.iterator.map(reflSubsets).reduce(_ union _)
      var result = newer
      for (oldRec <- older) {
        val totalOld = reflSubsets(oldRec)
        if (!totalOld.exists(totalNew.contains))
          result += oldRec
      }
      result
    }

  val reflSubsetFn: (LogicVer, HashScheme, HashScope) => Set[HashScope] = {
    import HashScope._
    type Lookup = HashScope => Set[HashScope]

    def mkReflSubsets(directNonReflSubsets: Lookup): Lookup = {
      val tc = TransitiveClosure.auto(all.whole)(directNonReflSubsets, _ => true)
      all.iterator.map(s => (s, tc(s))).toMap.apply
    }

    val latest = mkReflSubsets {
      case WholeProject    => Set(Config, Content)
      case Config          => Set(CfgIssueTypes, CfgReqTypes, CfgFields, CfgTags)
      case Content         => Set(Reqs, ReqCodes, TextFieldData, TagData, ImplicationData, DeletionReasons)
      case CfgIssueTypes
         | CfgReqTypes
         | CfgFields
         | CfgTags
         | Reqs
         | ReqCodes
         | TextFieldData
         | TagData
         | ImplicationData
         | DeletionReasons => Set.empty
    }

    (_, _, scope) => latest(scope)
  }

  val reflSubsets: HashRec => Set[HashScope] =
    r => reflSubsetFn(r.logicVer, r.scheme, r.scope)
}
