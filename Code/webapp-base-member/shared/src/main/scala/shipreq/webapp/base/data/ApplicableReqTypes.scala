package shipreq.webapp.base.data

import shipreq.base.util._

// Not a case class because even though I wrote my own apply method in the object,
// in RandomData Gen.apply2(ApplicableReqTypes.apply) would somehow end up bypassing my apply method.
// RandomData Gen.apply2(ApplicableReqTypes(_, _)) would work but it's too risky to leave in the codebase.

final class ApplicableReqTypes private[ApplicableReqTypes](val applicability: Applicability,
                                                           val reqTypes     : Set[ReqTypeId]) {

  @elidable(elidable.INFO)
  override def toString =
    s"ApplicableReqTypes($applicability, $reqTypes)"

  override def hashCode =
    applicability.hashCode + reqTypes.hashCode * 31

  override def equals(obj: Any): Boolean =
    obj match {
      case b: ApplicableReqTypes => (applicability ==* b.applicability) && (reqTypes ==* b.reqTypes)
      case _                     => false
    }

  def isEmpty: Boolean =
    reqTypes.isEmpty

  @inline def nonEmpty: Boolean =
    !isEmpty

  @inline def apply(id: ReqTypeId): Applicability =
    asFn(id)

  val asFn: ReqTypeId => Applicability =
    if (isEmpty)
      _ => Applicable
    else
      id => applicability.when(reqTypes contains id)

  def hardDelete(id: CustomReqTypeId): ApplicableReqTypes =
    ApplicableReqTypes(applicability, reqTypes - id)

  def filterReqTypes(l: Live, reqTypes: ReqTypes): ApplicableReqTypes =
    filterReqTypes(reqTypes.get(_).exists(_.live is l))

  def filterReqTypes(f: ReqTypeId => Boolean): ApplicableReqTypes =
    ApplicableReqTypes(applicability, reqTypes filter f)

  /** Assuming that this only contains live data (especially when coming from the UI), this will carry over the dead
    * data from the previous value.
    */
  def withDeadFrom(previous: ApplicableReqTypes, cfg: ReqTypes): ApplicableReqTypes =
    if (previous.nonEmpty && applicability ==* previous.applicability) {
      // User changes values but not applicability, retain the old dead items that were hidden to them
      val oldDeadStuff = previous.reqTypes.filter(cfg.live(_, Live).is(Dead))
      ApplicableReqTypes(applicability, reqTypes ++ oldDeadStuff)
    } else
      this
}

object ApplicableReqTypes {
  implicit def univEq: UnivEq[ApplicableReqTypes] = UnivEq.force

  val empty: ApplicableReqTypes =
    new ApplicableReqTypes(NotApplicable, Set.empty)

  def apply(applicability: Applicability, reqTypes: Set[ReqTypeId]): ApplicableReqTypes =
    if (reqTypes.isEmpty)
      empty
    else
      new ApplicableReqTypes(applicability, reqTypes)

  def whitelist(ids: ReqTypeId*): ApplicableReqTypes =
    apply(Applicable, ids.toSet)

  def blacklist(ids: ReqTypeId*): ApplicableReqTypes =
    apply(NotApplicable, ids.toSet)
}