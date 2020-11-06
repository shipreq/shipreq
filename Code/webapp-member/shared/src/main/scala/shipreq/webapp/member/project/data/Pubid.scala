package shipreq.webapp.member.project.data

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import monocle.Prism
import nyaya.util.Multimap
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.member.project.text.Grammar

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

  val StringPrism: Prism[String, ExternalPubid] =
    Grammar.pubid.stringPrism

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

  def allocGR(reqTypeId: CustomReqTypeId)(reqId: ReqIdC): (PubidRegister, PubidC) =
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

  /** Finds the req type that has more reqs than any other, and when available, also returns the range of pubid numbers
    * that belong only to this req type.
    *
    * Eg. if you had:
    *   - MF 1 to  8
    *   - FR 1 to 11
    *   - UC 1 to  6
    * the result would be FR 9-11
    *
    * Eg. if you had:
    *   - MF 1 to 11
    *   - FR 1 to 11
    *   - UC 1 to  6
    * the result would be None
    */
  lazy val uniquePositions: Option[PubidRegister.UniquePositions] = {
    import PubidRegister.UniquePositions

    val top2 =
      MutableArray(value.iterator.filter(_._2.nonEmpty))
        .sortBy(_._2.length)(Ordering.Int.reverse)
        .iterator()
        .take(2)
        .toList

    top2 match {
      case (rt, v) :: Nil                                  => Some(UniquePositions(rt, 1, v.length))
      case (rt, v) :: (_, v2) :: _ if v.length > v2.length => Some(UniquePositions(rt, v2.length + 1, v.length))
      case _                                               => None
    }
  }

  lazy val highestPosition: Int =
    if (value.isEmpty)
      0
    else
      value.iterator.map(_._2.length).max
}

object PubidRegister {
  implicit def univEq: UnivEq[PubidRegister] =
    UnivEq.derive

  def emptyMM: Multimap[ReqTypeId, Vector, ReqId] =
    UnivEq.emptyMultimap

  def empty: PubidRegister =
    PubidRegister(emptyMM)

  final case class UniquePositions(reqTypeId: ReqTypeId, first: Int, last: Int) {
    assert(first >= 1 && first <= last, s"Nope: $this")

    def lookup(pos: Int): Option[Pubid] =
      Option.when(pos >= first && pos <= last)(PubidT(reqTypeId, ReqTypePos(pos)))
  }

  implicit def univEqUniquePositions: UnivEq[UniquePositions] =
    UnivEq.derive

}
