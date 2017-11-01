package shipreq.webapp.base.hash2

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq.UnivEq
import shipreq.webapp.base.data.Project

sealed abstract class HashScope

object HashScope {
  sealed abstract class WithProjectAccess[@specialized(Int, Long, Char, Boolean) A](f: Project => A) extends HashScope {
    final def -->(h: HashFn[A]): (this.type, HashFn[Project]) =
      (this, h contramap f)
  }

  case object ProjectName     extends WithProjectAccess(_.name)
  case object CfgIssueTypes   extends WithProjectAccess(_.config.customIssueTypes)
  case object CfgReqTypes     extends WithProjectAccess(_.config.reqTypes)
  case object CfgFields       extends WithProjectAccess(_.config.fields)
  case object CfgTags         extends WithProjectAccess(_.config.tags)
  case object GenericReqs     extends WithProjectAccess(_.reqs.genericReqs)
  case object UseCases        extends WithProjectAccess(_.reqs.useCases)
  case object PubidRegister   extends WithProjectAccess(_.reqs.pubids)
  case object ReqCodes        extends WithProjectAccess(_.reqCodes)
  case object TextFieldData   extends WithProjectAccess(_.reqText)
  case object TagData         extends WithProjectAccess(_.reqTags)
  case object ImplicationData extends WithProjectAccess(_.implications)
  case object DeletionReasons extends WithProjectAccess(_.deletionReasons)
  case object SavedViews      extends WithProjectAccess(_.reqtableViews)

  implicit def univEq: UnivEq[HashScope] = UnivEq.force

  // type To[A] = Map[HashScope, A]
  final case class To[A](private val map: Map[HashScope, A]) extends AnyVal {
    def isEmpty: Boolean = map.isEmpty
    def nonEmpty: Boolean = map.nonEmpty

    def contains(s: HashScope): Boolean =
      map.contains(s)

    def get(s: HashScope): Option[A] =
      map.get(s)

    def need(s: HashScope): A =
      map(s)

    def scopeIterator: Iterator[HashScope] =
      map.keysIterator

    def map[B](f: A => B): To[B] =
      To(map.mapValuesNow(f))

    def mapOrRemoveEntries[B](f: (HashScope, A) => Option[(HashScope, B)]): To[B] =
      To(map.mapOrRemoveEntries(f))

    def foreach(f: ((HashScope, A)) => Unit): Unit =
      map.foreach(f)

//    def mapWithScope[B](f: (HashScope, A) => B): To[B] =
//      To(map.mapEntriesNow((k, v) => (k, f(k, v))))

    def updated(s: HashScope, a: A): To[A] =
      To(map.updated(s, a))

    def -(s: HashScope): To[A] =
      To(map - s)
  }

  final case class Version(value: Int) {
    def inc = Version(value + 1)
  }

  object Version {
    val init = apply(1)

    implicit val ordering: Ordering[Version] =
      Ordering.by(_.value)
  }

  final case class VersionedHashFn(version: HashScope.Version, hashFn: HashFn[Project]) {
    def evolve(hashFn: HashFn[Project]): VersionedHashFn =
      VersionedHashFn(version.inc, hashFn)
  }

  object VersionedHashFn {
    def init(hashFn: HashFn[Project]): VersionedHashFn =
      apply(Version.init, hashFn)

    implicit val ordering: Ordering[VersionedHashFn] =
      Ordering.by(_.version)
  }

  type VersionedHashFns = To[VersionedHashFn]

}
