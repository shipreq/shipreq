package shipreq.webapp.base.hash2

import japgolly.univeq.UnivEq
import shipreq.base.util.EqualsByRef
import shipreq.webapp.base.data.Project

final case class HashSchemeId(value: Char) extends AnyVal {
  def plus(n: Int): HashSchemeId =
    HashSchemeId((value.toInt + n).toChar)
}

object HashSchemeId {

  implicit def univEq: UnivEq[HashSchemeId] =
    UnivEq.derive

  val zero: HashSchemeId =
    apply('a')
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

final case class HashScheme(id: HashSchemeId, hashFns: HashScope.VersionedHashFns) extends EqualsByRef {

  override def toString = s"HashScope(${id.value})"

  def hash(p: Project): HashScope.To[Int] =
    hashFns.map(_.hashFn(p))
}

object HashScheme {
  def withoutId(hashFns: HashScope.VersionedHashFns): HashSchemeId => HashScheme =
    apply(_, hashFns)
}