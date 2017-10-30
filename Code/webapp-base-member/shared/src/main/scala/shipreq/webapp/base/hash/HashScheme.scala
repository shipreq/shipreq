package shipreq.webapp.base.hash

import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util._
import shipreq.base.util.univeq._

final case class HashSchemeId(value: Char) extends AnyVal

/**
 * A means of turning data into ints in a way that is stable and deterministic, and resilient to any future changes
 * (so changes to data structure, hashing algorithm, hash scope).
 *
 * To add a new hash scheme because data structures have changed, do the following:
 *
 * - Create a new `DataHasherVₙ` class.
 *
 * - Append the new `DataHasherVₙ` class to `HashScheme.allOldToNew`.
 *
 * - Update the `HashScheme.Latest` type alias.
 *
 * - Update `HashSchemeTest`.
 */
final case class HashScheme private[hash](hasher: DataHasher, id: HashSchemeId, invalidScopes: Set[HashScope]) {
  override val hashCode = hasher.##
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
   */
  val allOldToNew: NonEmptyVector[HashScheme] = {
    var i = 'a'.toInt - 1

    def make(d: DataHasher)(scopeValidity: HashScope => Validity): HashScheme = {
      i += 1
      val id = HashSchemeId(i.toChar)
      val invalidScopes = HashScope.all.iterator.filter(scopeValidity(_) is Invalid).toSet
      HashScheme(d, id, invalidScopes)
    }

    import HashScope._

    // APPEND-ONLY. DO NOT ALTER POSITION OF EXISTING ENTRIES.
    NonEmptyVector(
      make(new DataHasherV1(MurmurHash3)) {
        case HashScope.Other
           | HashScope.Content => Invalid
        case _                 => Valid
      },
      make(new DataHasherV2(MurmurHash3))(_ => Valid),
    )
  }

  type Latest = DataHasherV2

  val latest: HashScheme =
    allOldToNew.last

  private[this] val allWhole = allOldToNew.whole

  def unsafeGet(id: HashSchemeId): HashScheme =
    allWhole(id.value - 'a')
}
