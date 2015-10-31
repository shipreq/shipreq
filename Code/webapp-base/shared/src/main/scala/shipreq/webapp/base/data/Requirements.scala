package shipreq.webapp.base.data

import monocle.Traversal
import nyaya.util.Multimap
import monocle.macros.Lenses
import scalaz.Equal
import scalaz.std.stream.streamInstance
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.text.Text, Text.Equality._
import shipreq.webapp.base.util.Must._
import DataImplicits._

/**
 * The ID of a top-level, or sub- requirement.
 */
sealed trait ReqOrSubReqId extends TaggedInt

/**
 * The ID of a sub-requirement.
 *
 * A sub-requirement is a requirement that is a constituent of a larger requirement.
 *
 * Example: a use-case is a top-level requirement, and has steps which are sub-requirements.
 */
sealed trait SubReqId extends ReqOrSubReqId


/**
 * The ID of a top-level requirement.
 *
 * The `T` suffix means typed with `ReqId` being `ReqIdT[_]`.
 */
sealed trait ReqIdT[+RT <: ReqTypeId] extends ReqOrSubReqId

/**
 * An abstract requirement.
 *
 * The `T` suffix means typed with `Req` being `ReqT[_]`.
 */
sealed abstract class ReqT[+RT <: ReqTypeId] {
  val id: ReqIdT[RT]
  val pubid: PubidT[RT]

  def live(customReqTypes: CustomReqTypeIMap): Live

  @inline final def reqTypeId: RT =
    pubid.reqTypeId
}

object ReqT {
  object IdAccess extends ObjDataId[ReqT.type, Req, ReqId] {
    override def id(d: Req) = d.id
    override val unapplyData: AnyRef => Option[Req] = {case r: Req => Some(r); case _ => None}
  }
}

// =====================================================================================================================
// Generic Req

final case class GenericReqId(value: Int) extends ReqIdT[CustomReqTypeId]

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

// =====================================================================================================================
// Use Case

final case class UseCaseId(value: Int) extends ReqIdT[StaticReqType.UseCase]

@Lenses
final case class UseCase(id            : UseCaseId,
                         pos           : ReqTypePos,
                         title         : Text.UseCaseTitle.OptionalText,
                         stepsNA       : UseCase.Steps,
                         stepsE        : UseCase.Steps,
                         liveExplicitly: Live) extends ReqT[StaticReqType.UseCase] {

  override val pubid: PubidT[StaticReqType.UseCase] =
    PubidT(StaticReqType.UseCase, pos)

  /**
   * For cases when you know you have a [[UseCase]] (instead of a [[Req]]) and you want the *total* live value (as
   * opposed to the explicit live value).
   */
  @inline def liveUC: Live =
    liveExplicitly

  override def live(customReqTypes: CustomReqTypeIMap): Live =
    liveUC

  val stepsWithCtx: Stream[UseCaseStepWithCtx] = {
    def go(field: StaticField.UseCaseStepTree, tree: UseCase.Steps) =
      tree.locAndValueIterator(UseCaseStepWithCtx(this, field, _, _))

    go(StaticField.NormalAltStepTree, stepsNA).toStream append
      go(StaticField.ExceptionStepTree, stepsE)
  }

  def stepIterator: Iterator[UseCaseStep] =
    stepsNA.valueIterator ++ stepsE.valueIterator
}

object UseCase {
  object IdAccess extends ObjDataId[UseCase.type, UseCase, UseCaseId] {
    override def id(d: UseCase) = d.id
    override val unapplyData: AnyRef => Option[UseCase] = {case r: UseCase => Some(r); case _ => None}
  }

  type Steps = VectorTree[UseCaseStep]

  val stepsTraversal =
    Traversal.apply2[UseCase, UseCase.Steps](_.stepsNA, _.stepsE)((na, e, uc) => uc.copy(stepsNA = na, stepsE = e))

  @inline def emptySteps: Steps =
    VectorTree.empty

  implicit def stepEquality: UnivEq[UseCaseStep] = UnivEq.derive
  implicit def equality    : UnivEq[UseCase]     = UnivEq.derive
}

case class UseCaseStepId(value: Int) extends SubReqId

@Lenses
case class UseCaseStep(id: UseCaseStepId, title: Text.UseCaseStep.OptionalText)

object UseCaseStep {
  object IdAccess extends ObjDataId[UseCaseStep.type, UseCaseStep, UseCaseStepId] {
    override def id(d: UseCaseStep) = d.id
    override val unapplyData: AnyRef => Option[UseCaseStep] = {case r: UseCaseStep => Some(r); case _ => None}
  }
}

/**
 * A [[UseCaseStep]] with context that clarifies it when viewed from at project-level, rather than the use-case-level.
 *
 * Always generated; never stored.
 */
case class UseCaseStepWithCtx(useCase: UseCase,
                              field  : StaticField.UseCaseStepTree,
                              loc    : VectorTree.Location,
                              step   : UseCaseStep) {
  @inline def useCaseId = useCase.id
  @inline def stepId    = step.id
}

object UseCaseStepWithCtx {
  object IdAccess extends ObjDataId[UseCaseStepWithCtx.type, UseCaseStepWithCtx, UseCaseStepId] {
    override def id(d: UseCaseStepWithCtx) = d.stepId
    override val unapplyData: AnyRef => Option[UseCaseStepWithCtx] = {case r: UseCaseStepWithCtx => Some(r); case _ => None}
  }
}

// ---------------------------------------------------------------------------------------------------------------------
// Collective

object Requirements {
  def empty = Requirements(emptyDataMap(GenericReq), emptyDataMap(UseCase), PubidRegister.empty)

  implicit lazy val equality: Equal[Requirements] = UtilMacros.deriveEqual
}

@Lenses
case class Requirements(genericReqs: GenericReqIMap,
                        useCases   : UseCaseIMap,
                        pubids     : PubidRegister) {

  lazy val reqs: IMap[ReqId, Req] =
    IMap.empty[ReqId, Req](_.id) ++
      genericReqs.valuesIterator ++
      useCases.valuesIterator

  // This may be used in cases where calculating useCaseSteps will be a waste of time and memory.
  // The penalty is that no contextual info is preserved.
  def useCaseStepIterator: Iterator[UseCaseStep] =
    useCases.valuesIterator.flatMap(_.stepIterator)

  lazy val useCaseSteps: UseCaseStepIMap =
    useCases.valuesIterator.foldLeft(emptyDataMap(UseCaseStepWithCtx))((m, uc) =>
      m addAllF uc.stepsWithCtx)

  def isEmpty = reqs.isEmpty
  def nonEmpty = !isEmpty

  def getReq[T <: ReqTypeId](id: ReqIdT[T]): Option[ReqT[T]] =
    id match {
      case i: GenericReqId => genericReqs.get(i)
      case i: UseCaseId    => useCases   .get(i)
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
    reqs.valuesIterator.foldLeft(UnivEq.emptyMultimap[ReqTypeId, Vector, Req])((q, r) =>
      q.add(r.reqTypeId, r))
}