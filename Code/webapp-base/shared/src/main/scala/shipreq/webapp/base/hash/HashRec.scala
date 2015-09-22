package shipreq.webapp.base.hash

import shipreq.base.util.UnivEq
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.ApplyEvent.LogicVer

/**
 * Hash record.
 */
case class HashRec(scope   : HashScope,
                   logicVer: LogicVer,
                   scheme  : HashScheme,
                   hash    : Int) {

  def recalc(p: Project): Int =
    HashScope.hash(scope, scheme.value, p)

  def isValid(p: Project): Boolean =
    recalc(p) == hash
}

object HashRec {
  type Collection = Set[HashRec]

  implicit def equality: UnivEq[HashRec] = UnivEq.derive
  implicit def collectioneEquality: UnivEq[Collection] = UnivEq.univEqSet

  def changes(p1: Project, p2: Project): HashRec.Collection =
    changes(HashScope.defaultSet.whole, LogicVer.Current, HashScheme.latest, p1, p2)

  private def changes(scopes: TraversableOnce[HashScope], lv: LogicVer, scheme: HashScheme, p1: Project, p2: Project): HashRec.Collection = {
    var r: HashRec.Collection = Set.empty
    val d = scheme.value
    for (s <- scopes) {
      val h1 = HashScope.hash(s, d, p1)
      val h2 = HashScope.hash(s, d, p2)
      if (h1 != h2)
        r += HashRec(s, lv, scheme, h2)
    }
    r
  }

  // HashRec.merge ignores LogicVer (because it hasn't changed yet)
  def merge(as: Collection, bs: Collection): Collection =
    if (bs.isEmpty)
      // No records = no validation. Anything could change. Ignore all previous records.
      bs
    else {
      // Invalidate As which overlap with newer Bs
      var result = bs
      for (a <- as) {
        def noOverlap = !bs.exists(b => HashScope.overlap(a.scope, b.scope))
        if (noOverlap)
          result += a
      }
      result
    }
}