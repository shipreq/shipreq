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
      case Some(h) => Valid <~ (h ≟ recalc(p))
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
      if (h1 ≠ h2)
        r += HashRec(s, lv, scheme)(Some(h2))
    }
    r
  }

  // HashRec.merge ignores LogicVer (because it hasn't changed yet)
  def merge(earlier: Collection, later: Collection): Collection =
    if (earlier.isEmpty)
      later
    else if (later.isEmpty)
      earlier
    else {
      // Add non-overlapping old
      val totalNew = later.iterator.map(HashScope reflSubsets _.scope).reduce(_ union _)
      var result = later
      for (rec <- earlier) {
        val totalOld = HashScope reflSubsets rec.scope
        if (!totalOld.exists(totalNew.contains))
          result += rec
      }
      result
    }
}