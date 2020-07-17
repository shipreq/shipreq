package shipreq.webapp.base.data

import japgolly.microlibs.stdlib_ext.StdlibExt._
import monocle.macros.GenIso
import monocle.{Iso, Prism}
import nyaya.util.Multimap
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.text.Grammar

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

  /** The current, active [[ExternalPubid]] */
  def external(p: Project): ExternalPubid = {
    val rt = p.config.reqTypes.need(reqTypeId)
    ExternalPubid(rt.mnemonic, pos)
  }

  /** Past [[ExternalPubid]] for this Pubid.
    * Note that requirements can also have past-Pubids; this method doesn't check the [[PubidRegister]]. */
  def pastExternals(p: Project): Set[ExternalPubid] = {
    val rt = p.config.reqTypes.need(reqTypeId)
    rt.oldMnemonics.map(ExternalPubid(_, pos))
  }
}

object PubidT {
  implicit def equality[T <: ReqTypeId : UnivEq]: UnivEq[PubidT[T]] = UnivEq.derive
}

/**
 * A [[Pubid]] as seen from outside of a project, or from the user's perspective.
 */
final case class ExternalPubid(mnemonic: ReqType.Mnemonic, pos: ReqTypePos) {
  import ExternalPubid.LookupFailure

  def lookup(p: Project): LookupFailure \/ Req =
    lookup(p.config.reqTypes, p.content.reqs)

  def lookup(reqTypes: ReqTypes, reqs: Requirements): LookupFailure \/ Req =
    reqTypes.allByMnemonic.get(mnemonic) match {
      case None =>
        -\/(LookupFailure.InvalidReqType)
      case Some(rt) =>
        val i = pos.value - 1
        val register = reqs.pubids.value(rt.reqTypeId)
        if (register.isIndexValid(i))
          \/-(reqs need register(i))
        else
          -\/(LookupFailure.InvalidPos(rt, register.length))
    }
}

object ExternalPubid {
  implicit def equality: UnivEq[ExternalPubid] = UnivEq.derive

  val TupleIso: Iso[(ReqType.Mnemonic, ReqTypePos), ExternalPubid] =
    GenIso.fields[ExternalPubid].reverse

  val StringPrism: Prism[String, ExternalPubid] =
    Grammar.pubid.stringPrism ^<-> TupleIso

  def parse(s: String): Option[ExternalPubid] =
    StringPrism.getOption(s)

  @inline def preprocessor =
    Grammar.pubid.preprocessor

  implicit val ordering: Ordering[ExternalPubid] =
    Ordering.fromLessThan((a, b) =>
      a.mnemonic.value.compareTo(b.mnemonic.value) match {
        case 0 => a.pos.value < b.pos.value
        case n => n < 0
      }
    )

  sealed abstract class LookupFailure
  object LookupFailure {
    case object InvalidReqType extends LookupFailure
    case class InvalidPos(reqType: ReqType, maxLegalPos: Int) extends LookupFailure
  }
}

/**
 * Once a (reqtype x position) is allocated, it is never removed.
 * Thus, the 0-based position in the vector corresponds with 1-based [[ReqTypePos]] values.
 */
final case class PubidRegister(value: Multimap[ReqTypeId, Vector, ReqId]) {

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

  def all(id: ReqId): Set[Pubid] = {
    value.iterator.flatMap {
      case (rt, ids) =>
        val i = ids.indexWhere(_ ==* id)
        if (i >= 0)
          PubidT(rt, ReqTypePos(i + 1)) :: Nil
        else
          Nil
    }.toSet
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
