package shipreq.webapp.base.hash2

import scala.collection.immutable.ListSet
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.ApplyEvent.LogicVer

final case class HashRecs(values: List[HashRecs.ByScheme]) extends AnyVal {
  def apply(scheme: HashScheme): Option[HashRecs.ByScheme] =
    values.find(_.scheme ==* scheme)
}

object HashRecs {

  final case class ByScheme(scheme: HashScheme, hashes: HashScope.To[Option[Int]])

  def empty: HashRecs =
    apply(Nil)

  private val latestScheme = HashSchemes.Registry.latest

//  def full(p: Project): HashRecs =
//    HashRecs(ByScheme(latestScheme, latestScheme.hash(p).map(Some(_))) :: Nil)

  def changes(p1: Project, p2: Project): HashRecs =
    __changes(latestScheme, p1, p2)

  /** Public for testing */
  def __changes(scheme: HashScheme, p1: Project, p2: Project): HashRecs = {
    var r = Map.empty[HashScope, Option[Int]]
    for (kv <- scheme.hashFns) {
      val scope = kv._1
      val hashFn = kv._2.hashFn
      val h1 = hashFn(p1)
      val h2 = hashFn(p2)
      if (h1 !=* h2)
        r += scope -> Some(h2)
    }
    HashRecs(ByScheme(scheme, HashScope.To(r)) :: Nil)
  }

}


///**
// * Single hash record.
// *
// * Its equality and hashCode exclude the hash itself.
// *
// * @param hash `None` means "disable integrity checking".
// */
//final case class HashRec(scheme: HashScheme,
//                         scope : HashScope)(
//                     val hash  : Option[Int]) {
//
//  def logicVer: LogicVer =
//    LogicVer.SoleInstance
//
//  override def toString =
//    s"HashRec($scope, $logicVer, $scheme)(${hash.fold("∅")(_.toString)})"
//
////  def recalc(p: Project): Int =
////    scheme.hasher(scope, p)
////
////  def validate(p: Project): Validity =
////    Valid when validateF(p).isEmpty
//
////  def validateF(p: Project): List[HashDiscrepancy]] =
////    if (logicVer.isCurrent)
////      hash.flatMap { e =>
////        val a = recalc(p)
////        if (e ==* a)
////          None
////        else
////          Some(ValidationFailure(expect = e, actual = a))
////      }
////    else
////      // Can't validate old logic; new logic is always applied.
////      None
//}
//
//object HashRec {
//
//  implicit def equality: UnivEq[HashRec] = UnivEq.derive
//
//  type Collection = ListSet[HashRec]
//
//  object Collection {
//    implicit def UnivEqCollection: UnivEq[Collection] = UnivEq.univEqListSet
