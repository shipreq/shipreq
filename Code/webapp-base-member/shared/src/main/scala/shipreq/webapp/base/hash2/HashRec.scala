package shipreq.webapp.base.hash2

import scala.collection.immutable.ListSet
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.ApplyEvent.LogicVer

/**
 * Single hash record.
 *
 * Its equality and hashCode exclude the hash itself.
 *
 * @param hash `None` means "disable integrity checking".
 */
final case class HashRec(scheme: HashScheme,
                         scope : HashScope)(
                     val hash  : Option[Int]) {

  def logicVer: LogicVer =
    LogicVer.SoleInstance

  override def toString =
    s"HashRec($scope, $logicVer, $scheme)(${hash.fold("∅")(_.toString)})"

//  def recalc(p: Project): Int =
//    scheme.hasher(scope, p)
//
//  def validate(p: Project): Validity =
//    Valid when validateF(p).isEmpty

//  def validateF(p: Project): List[HashDiscrepancy]] =
//    if (logicVer.isCurrent)
//      hash.flatMap { e =>
//        val a = recalc(p)
//        if (e ==* a)
//          None
//        else
//          Some(ValidationFailure(expect = e, actual = a))
//      }
//    else
//      // Can't validate old logic; new logic is always applied.
//      None
}

object HashRec {

  implicit def equality: UnivEq[HashRec] = UnivEq.derive

  type Collection = ListSet[HashRec]

  object Collection {
    implicit def UnivEqCollection: UnivEq[Collection] = UnivEq.univEqListSet

    val empty: Collection =
      ListSet.empty

    private val latestScheme = HashSchemes.Registry.latest

    def ofHashes(scheme: HashScheme, hs: HashScope.To[Int]): HashRec.Collection = {
      ???
    }

    def full(p: Project): HashRec.Collection =
      ofHashes(latestScheme, latestScheme.hash(p))

    def changes(p1: Project, p2: Project): HashRec.Collection =
      __changes(latestScheme, p1, p2)

    /** Public for testing */
    def __changes(scheme: HashScheme, p1: Project, p2: Project): HashRec.Collection = {
      var r = empty
      for (kv <- scheme.hashFns) {
        val scope = kv._1
        val hashFn = kv._2.hashFn
        val h1 = hashFn(p1)
        val h2 = hashFn(p2)
        if (h1 !=* h2)
          r += HashRec(scheme, scope)(Some(h2))
      }
      r
    }

//    def merge(older: Collection, newer: Collection): Collection =
//      if (older.isEmpty)
//        newer
//      else if (newer.isEmpty)
//        older
//      else {
//        ???
//      }
  }

//  def merge(older: Collection, newer: Collection): Collection =
//    // Any existence of old logic completely invalidates past results because there's (currently) no way of determining
//    // how the change in logic impacts previously-generated hashes. Therefore, we throw out all old hashes.
//    if (older.isEmpty || newer.exists(!_.logicVer.isCurrent))
//      newer
//    else if (newer.isEmpty)
//      older
//    else {
//      // Add non-overlapping old
//
//      val totalNew = {
//        val b = Set.newBuilder[HashScope]
//        for (n <- newer) {
//          b ++= reflSubsets(n)
//          b ++= n.scheme.invalidScopes
//        }
//        b.result()
//      }
//      val totalNewContains = totalNew.contains _
//
//      var result = newer
//      for (oldRec <- older) {
//        val totalOld = reflSubsets(oldRec)
//        if (!totalOld.exists(totalNewContains))
//          result += oldRec
//      }
//      result
//    }
}
