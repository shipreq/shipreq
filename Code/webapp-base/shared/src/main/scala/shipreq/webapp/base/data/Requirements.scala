package shipreq.webapp.base.data

import nyaya.util.Multimap
import monocle.macros.Lenses
import scalaz.Equal
import scalaz.syntax.equal._
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.text.Text, Text.Equality._
import shipreq.webapp.base.util.Must._
import DataImplicits._

// ===================================================================================================================
// Public IDs (like MF-3)

/**
 * A position (ordinal) in a req-type's ordered list of requirements.
 *
 * Eg. the "3" in "FR-3".
 *
 * @param value ≥ 1.
 */
final case class ReqTypePos(value: Int) extends TaggedInt

/**
 * Public ID: A requirement's ID from the public's point-of-view.
 *
 * Eg. "FR-3"
 */
final case class PubidT[+T <: ReqTypeId](reqTypeId: T, pos: ReqTypePos)

object PubidT {
  implicit def equality[T <: ReqTypeId : UnivEq]: UnivEq[PubidT[T]] = UnivEq.derive
}

/**
 * Once a (reqtype x position) is allocated, it is never removed.
 * Thus, the 0-based position in the vector corresponds with 1-based [[ReqTypePos]] values.
 */
case class PubidRegister(value: Multimap[ReqTypeId, Vector, ReqId]) {

  def allocC(reqTypeId: CustomReqTypeId)(reqId: ReqIdC): (PubidRegister, PubidC) =
    _alloc(reqTypeId)(reqId)

  private def _alloc[T <: ReqTypeId](reqTypeId: T)(reqId: ReqIdT[T]): (PubidRegister, PubidT[T]) = {
    val cur = value(reqTypeId)
    val i = cur.indexWhere(_ ≟ reqId)
    if (i >= 0)
      (this, PubidT(reqTypeId, ReqTypePos(i + 1)))
    else
      (PubidRegister(value.add(reqTypeId, reqId)), PubidT(reqTypeId, ReqTypePos(cur.size + 1)))
  }

   def apply[T <: ReqTypeId](id: PubidT[T]): Option[ReqIdT[T]] = {
    val v = value(id.reqTypeId)
    val i = id.pos.value - 1
    @inline def cast(r: ReqId) = r.asInstanceOf[ReqIdT[T]]
    try {
      Some(cast(v(i)))
    } catch {
      case _: IndexOutOfBoundsException => None
    }
  }
}

object PubidRegister {
  implicit def equality: UnivEq[PubidRegister] = UnivEq.derive
  def emptyMM: Multimap[ReqTypeId, Vector, ReqId] = UnivEq.emptyMultimap
  def empty = PubidRegister(emptyMM)
}

// =====================================================================================================================
// Requirements

/** type [[ReqIdT]] = [[GenericReqId]] */
sealed trait ReqIdT[+RT <: ReqTypeId] extends TaggedInt

/** [[Req]] = [[GenericReq]] */
sealed abstract class ReqT[+RT <: ReqTypeId] {
  val id: ReqIdT[RT]
  val pubid: PubidT[RT]

  def live(customReqTypes: CustomReqTypeIMap): Live

  @inline final def reqTypeId: RT =
    pubid.reqTypeId
}

object ReqT {
  implicit def equalReq(implicit g: UnivEq[GenericReq]): UnivEq[Req] = UnivEq.force

  object IdAccess extends ObjDataId[ReqT.type, Req, ReqId] {
    override def id(d: Req) = d.id
    override val unapplyData: AnyRef => Option[Req] = {case r: Req => Some(r); case _ => None}
  }
}

// ---------------------------------------------------------------------------------------------------------------------
// Generic

final case class GenericReqId(value: Int) extends TaggedInt with ReqIdT[CustomReqTypeId]

/**
 * A generic/low-level requirement comprised, primarily, of a custom req type and a title.
 *
 * @param liveExplicitly Whether the user has explicitly marked this req as deleted or not.
 */
@Lenses
final case class GenericReq(id            : GenericReqId,
                            pubid         : PubidC,
                            title         : Text.GenericReqTitle.OptionalText,
                            liveExplicitly: Live) extends ReqT[CustomReqTypeId] {

  import GenericReq.ImplicitLiveStatus

  def implicitLiveStatus(customReqTypes: CustomReqTypeIMap): ImplicitLiveStatus =
    customReqTypes.need(pubid.reqTypeId).live match {
      case Live => ImplicitLiveStatus.NoImpact
      case Dead => ImplicitLiveStatus.ReqTypeIsDead
    }

  override def live(customReqTypes: CustomReqTypeIMap): Live =
    liveExplicitly && implicitLiveStatus(customReqTypes).live
}

object GenericReq {
  implicit def equality: UnivEq[GenericReq] = UnivEq.derive

  object IdAccess extends ObjDataId[GenericReq.type, GenericReq, GenericReqId] {
    override def id(d: GenericReq) = d.id
    override val unapplyData: AnyRef => Option[GenericReq] = {case r: GenericReq => Some(r); case _ => None}
  }

  /**
   * In order for a requirement to be live, its dependencies must allow it.
   *
   * This encodes the impact of a requirement's dependencies on its live status.
   */
  sealed trait ImplicitLiveStatus {
    def live: Live
  }
  object ImplicitLiveStatus {
    case object NoImpact      extends ImplicitLiveStatus { override def live = Live }
    case object ReqTypeIsDead extends ImplicitLiveStatus { override def live = Dead }
  }
}

// ---------------------------------------------------------------------------------------------------------------------
// Collective

object Requirements {
  def empty = Requirements(emptyDataMap(GenericReq), PubidRegister.empty)

  implicit lazy val equality: Equal[Requirements] = UtilMacros.deriveEqual
}

@Lenses
case class Requirements(genericReqs: GenericReqIMap, pubids: PubidRegister) {

  val reqs: IMap[ReqId, Req] =
    // Temporary. Will do this properly when next Req type added
    genericReqs.asInstanceOf[IMap[ReqId, Req]]

  def isEmpty = reqs.isEmpty
  def nonEmpty = !isEmpty

  def getReq[T <: ReqTypeId](id: ReqIdT[T]): Option[ReqT[T]] =
    id match {
      case i: GenericReqId => genericReqs.get(i)
    }

  def getReqByPubid[T <: ReqTypeId](id: PubidT[T]): Option[ReqT[T]] =
    pubids(id) flatMap getReq

  def req[T <: ReqTypeId](id: ReqIdT[T]): ReqT[T] =
    getReq(id) mustExistElse s"Req $id not found."

  def reqByPubid[T <: ReqTypeId](id: PubidT[T]): ReqT[T] =
    getReqByPubid(id) mustExistElse s"Req for $id not found."

  def reqIdByPubid[T <: ReqTypeId](id: PubidT[T]): ReqIdT[T] =
    pubids(id) mustExistElse s"Req for $id not found."

  lazy val reqsByType: Multimap[ReqTypeId, Vector, Req] =
    UnivEq.emptyMultimap[ReqTypeId, Vector, Req]
      .addPairs(reqs.vstream(_.mapStrengthL(_.reqTypeId)): _*)
}