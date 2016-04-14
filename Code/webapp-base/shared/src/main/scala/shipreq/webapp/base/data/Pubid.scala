package shipreq.webapp.base.data

import nyaya.util.Multimap
import shipreq.base.util.univeq._
import shipreq.base.util.TaggedTypes._

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
 * The `T` suffix means typed with `Pubid` being `PubidT[_]`.
 *
 * Eg. "FR-3"
 */
final case class PubidT[+T <: ReqTypeId](reqTypeId: T, pos: ReqTypePos) {
  def external(p: Project): ExternalPubid = {
    val rt = p.config.reqType(reqTypeId)
    ExternalPubid(rt.mnemonic, pos)
  }
}

object PubidT {
  implicit def equality[T <: ReqTypeId : UnivEq]: UnivEq[PubidT[T]] = UnivEq.derive
}

/**
 * A [[Pubid]] as seen from outside of a project, or from the user's perspective.
 */
final case class ExternalPubid(mnemonic: ReqType.Mnemonic, pos: ReqTypePos)
object ExternalPubid {
  implicit def equality: UnivEq[ExternalPubid] = UnivEq.derive
}

/**
 * Once a (reqtype x position) is allocated, it is never removed.
 * Thus, the 0-based position in the vector corresponds with 1-based [[ReqTypePos]] values.
 */
case class PubidRegister(value: Multimap[ReqTypeId, Vector, ReqId]) {

  // TODO Should be GR not C
  def allocC(reqTypeId: CustomReqTypeId)(reqId: ReqIdC): (PubidRegister, PubidC) =
    _alloc(reqTypeId)(reqId)

  def allocUC(id: UseCaseId): (PubidRegister, PubidT[StaticReqType.UseCase]) =
    _alloc(StaticReqType.UseCase)(id)

  private def _alloc[T <: ReqTypeId](reqTypeId: T)(reqId: ReqIdT[T]): (PubidRegister, PubidT[T]) = {
    val cur = value(reqTypeId)
    val i = cur.indexWhere(_ ==* reqId)
    if (i >= 0)
      (this, PubidT(reqTypeId, ReqTypePos(i + 1)))
    else
      (PubidRegister(value.add(reqTypeId, reqId)), PubidT(reqTypeId, ReqTypePos(cur.size + 1)))
  }

  def apply[T <: ReqTypeId](id: PubidT[T]): Option[ReqIdT[T]] = {
    val v = value(id.reqTypeId)
    val i = id.pos.value - 1
    @inline def cast(r: ReqId) = r.asInstanceOf[ReqIdT[T]]
    try
      Some(cast(v(i)))
    catch {
      case _: IndexOutOfBoundsException => None
    }
  }

  // TODO Stop using same storage for ReqTypeId; too hard to encode in types. Have separate storage for GRs/UCs.
  def getUseCaseId(pos: ReqTypePos): Option[UseCaseId] = {
    val v = value(StaticReqType.UseCase)
    val i = pos.value - 1
    @inline def cast(r: ReqId) = r.asInstanceOf[UseCaseId]
    try
      Some(cast(v(i)))
    catch {
      case _: IndexOutOfBoundsException => None
    }
  }
}

object PubidRegister {
  implicit def equality: UnivEq[PubidRegister] =
    UnivEq.derive

  def emptyMM: Multimap[ReqTypeId, Vector, ReqId] =
    UnivEq.emptyMultimap

  def empty: PubidRegister =
    PubidRegister(emptyMM)
}
