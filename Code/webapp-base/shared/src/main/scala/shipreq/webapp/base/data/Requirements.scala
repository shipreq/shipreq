package shipreq.webapp.base.data

import monocle.{Iso, Traversal}
import monocle.macros.Lenses
import nyaya.util.Multimap
import scalaz.Equal
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
  val title: Text.AnyOptional

  def live(customReqTypes: CustomReqTypeIMap): Live

  @inline final def reqTypeId: RT =
    pubid.reqTypeId
}

object ReqT extends ReqTEquality {
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
                         stepsNA       : UseCaseSteps,
                         stepsE        : UseCaseSteps,
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

  def stepIterator: Iterator[UseCaseStep] =
    stepsNA.tree.valueIterator ++ stepsE.tree.valueIterator
}

object UseCase {
  object IdAccess extends ObjDataId[UseCase.type, UseCase, UseCaseId] {
    override def id(d: UseCase) = d.id
    override val unapplyData: AnyRef => Option[UseCase] = {case r: UseCase => Some(r); case _ => None}
  }

  val stepsTraversal =
    Traversal.apply2[UseCase, UseCaseSteps](_.stepsNA, _.stepsE)((na, e, uc) => uc.copy(stepsNA = na, stepsE = e))

  def empty(id: UseCaseId, pos: ReqTypePos, title: Text.UseCaseTitle.OptionalText, stepId: UseCaseStepId): UseCase =
    UseCase(id, pos, title, UseCaseSteps emptyRoot stepId, UseCaseSteps.empty, Live)

  implicit def equality: UnivEq[UseCase] = UnivEq.derive
}

case class UseCaseStepId(value: Int) extends SubReqId

@Lenses
case class UseCaseStep(id: UseCaseStepId, title: Text.UseCaseStep.OptionalText)

object UseCaseStep {
  object IdAccess extends ObjDataId[UseCaseStep.type, UseCaseStep, UseCaseStepId] {
    override def id(d: UseCaseStep) = d.id
    override val unapplyData: AnyRef => Option[UseCaseStep] = {case r: UseCaseStep => Some(r); case _ => None}
  }
  implicit def equality: UnivEq[UseCaseStep] = UnivEq.derive
}

@Lenses
case class UseCaseSteps(tree: UseCaseSteps.Tree) {
  lazy val withCtx: UseCaseStepWithCtx.ByStep =
    UseCaseStepWithCtx.emptyByStep ++
      tree.locAndValueIterator(UseCaseStepWithCtx.apply)
}

object UseCaseSteps {
  type Tree = VectorTree[UseCaseStep]
  implicit def equality: UnivEq[UseCaseSteps] = UnivEq.derive

  val empty: UseCaseSteps =
    UseCaseSteps(VectorTree.empty)

  def emptyRoot(id: UseCaseStepId): UseCaseSteps =
    single(UseCaseStep(id, Text.UseCaseStep.empty))

  def single(s: UseCaseStep): UseCaseSteps =
    UseCaseSteps(VectorTree single s)
}

/**
 * A [[UseCaseStep]] with context that clarifies it when viewed from at project-level, rather than the use-case-level.
 *
 * Always generated; never stored.
 */
case class UseCaseStepWithCtx(loc: VectorTree.Location, step: UseCaseStep) {
  @inline def stepId = step.id

//  def label(mnemonicPrefix: Boolean): String =
//    field.stepLabel(useCase.pos, loc, mnemonicPrefix)
}

object UseCaseStepWithCtx {
  object IdAccess extends ObjDataId[UseCaseStepWithCtx.type, UseCaseStepWithCtx, UseCaseStepId] {
    override def id(d: UseCaseStepWithCtx) = d.stepId
    override val unapplyData: AnyRef => Option[UseCaseStepWithCtx] = {case r: UseCaseStepWithCtx => Some(r); case _ => None}
  }

  type ByStep = IMap[UseCaseStepId, UseCaseStepWithCtx]

  val emptyByStep: ByStep =
    IMap.empty(_.stepId)
}

/**
 * @param stepIndex An index of all [[UseCaseStep]]s and the static portions of their locations.
 *                  This is calculable state which is normally never manually-managed, but is in this case due to the
 *                  frequency of step lookup and the ease of maintaining it (only two events affect it).
 * @param stepFlow Explicitly declared flow between steps.
 *                 Note that the position of steps provides implicit flow that isn't stored here (or anywhere).
 */
@Lenses
case class UseCases(imap: UseCaseIMap, stepIndex: UseCases.StepIndex, stepFlow: UseCases.StepFlow) {
  def stepIterator: Iterator[UseCaseStep] =
    imap.valuesIterator.flatMap(_.stepIterator)
}

object UseCases {

  /**
   * Information sufficient to uniquely identify a step tree within a project.
   */
  case class StepTreeKey(useCaseId: UseCaseId, field: StaticField.UseCaseStepTree)

  implicit def equalStepTreeKey: UnivEq[StepTreeKey] = UnivEq.derive

  /**
   * An index of all [[UseCaseStep]]s and the static portions of their locations.
   *
   * This is calculable state which is normally never manually-managed, but is in this case due to the frequency of step
   * lookup and the ease of maintaining it (very few events affect it).
   */
  type StepIndex = Map[UseCaseStepId, StepTreeKey]

  implicit def equalStepIndex: UnivEq[StepIndex] = UnivEq.univEqMap

  def emptyStepIndex: StepIndex = Map.empty

  def calcStepIndex(imap: UseCaseIMap): StepIndex = {
    var m = emptyStepIndex
    for {
      uc ← imap.valuesIterator
      id = uc.id
      f  ← StaticField.useCaseStepTrees
      s  ← f.useCaseStepTree.get(uc).valueIterator
    } m = m.updated(s.id, StepTreeKey(id, f))
    m
  }

  val StepFlow = new Digraph.Fix[UseCaseStepId]
  type StepFlow = StepFlow.BiDir

  implicit lazy val equality: Equal[UseCases] =
    UtilMacros.deriveEqual

  def empty: UseCases =
    UseCases(emptyDataMap(UseCase), emptyStepIndex, StepFlow.emptyBiDir)

  /**
   * A version [[UseCases]] that omits calculable state (i.e. [[StepIndex]]).
   */
  case class Stateless(imap: UseCaseIMap, stepFlow: StepFlow) {
    def withState: UseCases =
      statelessIso get this
  }

  val statelessIso: Iso[Stateless, UseCases] =
    Iso[Stateless, UseCases](
      s => UseCases(s.imap, calcStepIndex(s.imap), s.stepFlow))(
      u => Stateless(u.imap, u.stepFlow))
}

// ---------------------------------------------------------------------------------------------------------------------
// Collective

sealed trait ReqTEquality {
  implicit def equality: UnivEq[Req] = UnivEq.derive
}

object Requirements {
  implicit lazy val equality: Equal[Requirements] =
    UtilMacros.deriveEqual

  def empty: Requirements =
    Requirements(emptyDataMap(GenericReq), UseCases.empty, PubidRegister.empty)
}

@Lenses
case class Requirements(genericReqs: GenericReqIMap,
                        useCases   : UseCases,
                        pubids     : PubidRegister) {

  def isEmpty = reqs.isEmpty
  def nonEmpty = !isEmpty

  lazy val reqs: IMap[ReqId, Req] =
    IMap.empty[ReqId, Req](_.id) ++
      genericReqs.valuesIterator ++
      useCases.imap.valuesIterator

  def getReq[T <: ReqTypeId](id: ReqIdT[T]): Option[ReqT[T]] =
    id match {
      case i: GenericReqId => genericReqs.get(i)
      case i: UseCaseId    => useCases.imap.get(i)
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