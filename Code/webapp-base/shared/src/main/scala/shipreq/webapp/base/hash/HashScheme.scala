package shipreq.webapp.base.hash

import shipreq.base.util._

final case class HashSchemeId(value: Char) extends AnyVal

/**
 * A means of turning data into ints in a way that is stable and deterministic, and resilient to any future changes
 * (so changes to data structure, hashing algorithm, hash scope).
 *
 * To add a new hash scheme because data structures have changed, do the following:
 *
 * - Create a new `DataHasherVₙ` class to do what [[DataHasherCurrent]] used to before your data structure change.
 *   This will likely mean excluding a newly-added field.
 *
 * - Add the new `DataHasherVₙ` class to `HashScheme.raw`. It should be inserted before [[DataHasherCurrent]],
 *   i.e. second-last.
 *
 * - Update `HashSchemeTest`. Rename `latest` to `vₙ` and add a new `latest` test with values that consider the
 *   newly-added data structure changes.
 */
final case class HashScheme private[hash](value: DataHasher, id: HashSchemeId, invalidScopes: Set[HashScope]) {
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
   */
  val all: NonEmptyVector[HashScheme] = {

    var i = 'a'.toInt - 1

    def make(d: DataHasher)(scopeValidity: HashScope => Validity): HashScheme = {
      i += 1
      val id = HashSchemeId(i.toChar)
      val invalidScopes = HashScope.all.iterator.filter(scopeValidity(_) :: Invalid).toSet
      HashScheme(d, id, invalidScopes)
    }

    import HashScope._

    // APPEND-ONLY. DO NOT ALTER POSITION OF EXISTING ENTRIES.
    NonEmptyVector(
      make(new DataHasherV1(MurmurHash3)) {
        case WholeProject
           | Config
           | CfgIssueTypes
           | CfgReqTypes
           | CfgFields
           | CfgTags
           | Content
           | Reqs
           | ReqCodes
           | TextFieldData
           | TagData
           | ImplicationData => Valid
        case DeletionReasons => Invalid
      },
      make(new DataHasherCurrent(MurmurHash3))(_ => Valid))
  }

  val latest: HashScheme =
    all.last

  private[this] val allWhole = all.whole

  def unsafeGet(id: HashSchemeId): HashScheme =
    allWhole(id.value.toInt - 97)
}
