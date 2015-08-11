package shipreq.webapp.base.hash

import shipreq.base.util.{UnivEq, NonEmptyVector}
import shipreq.webapp.base.data.Project

final case class HashScheme private[HashScheme](value: DataHash, id: Short) {
  override val hashCode = value.##
  override def equals(o: Any): Boolean =
    o match {
      case b: AnyRef => b eq this
      case _         => false
    }
  override def toString = s"HashScheme($id)"

  def hash(project: Project): Int =
    value.hashProject hash project

  def apply(project: Project): ProjectHash =
    ProjectHash(this, hash(project))
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
    raw.mapWithIndex((h, i) => HashScheme(h, (i + 1).toShort))

  val latest: HashScheme =
    all.last

  private[this] val allWhole = all.whole

  def unsafeGet(id: Short): HashScheme =
    allWhole(id.toInt - 1)
}

case class ProjectHash(scheme: HashScheme, hash: Int)

object ProjectHash {
  implicit def equality: UnivEq[ProjectHash] = UnivEq.derive
}
