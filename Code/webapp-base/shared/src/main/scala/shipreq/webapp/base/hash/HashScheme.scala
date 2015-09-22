package shipreq.webapp.base.hash

import shipreq.base.util.{NonEmptyVector, UnivEq}

final case class HashSchemeId(value: Char) extends AnyVal

final case class HashScheme private[HashScheme](value: DataHash, id: HashSchemeId) {
  override val hashCode = value.##
  override def equals(o: Any): Boolean =
    o match {
      case b: AnyRef => b eq this
      case _         => false
    }
  override def toString = s"HashScheme(${id.value})"
}

object HashScheme {
  implicit def equality: UnivEq[HashScheme] = UnivEq.force

  /**
   * Order is oldest to most recent.
   *
   * APPEND-ONLY. DO NOT ALTER POSITION OF EXISTING ENTRIES.
   */
  private[this] val raw: NonEmptyVector[DataHash] =
    NonEmptyVector(new DataHash(MurmurHash3))

  val all: NonEmptyVector[HashScheme] =
    raw.mapWithIndex((h, i) => HashScheme(h, HashSchemeId((i + 97).toChar)))

  val latest: HashScheme =
    all.last

  private[this] val allWhole = all.whole

  def unsafeGet(id: HashSchemeId): HashScheme =
    allWhole(id.value.toInt - 97)
}
