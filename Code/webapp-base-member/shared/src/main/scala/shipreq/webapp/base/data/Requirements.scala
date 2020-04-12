package shipreq.webapp.base.data

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.microlibs.utils.{BiMap, Memo}
import monocle.{Iso, Traversal}
import monocle.macros.Lenses
import nyaya.util.Multimap
import scala.collection.View
import scalaz.{-\/, Equal, \/, \/-}
import shipreq.base.util._
import shipreq.base.util.TaggedTypes._
import shipreq.base.util.univeq._
import shipreq.webapp.base.text.{Text, UseCaseStepFlowText}
import shipreq.webapp.base.text.Text.Equality._
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
sealed trait ReqIdT[+RT <: ReqTypeId] extends ReqOrSubReqId {
  def foldReqId[A](gr: GenericReqId => A, uc: UseCaseId => A): A
}

/**
 * An abstract requirement.
 *
 * The `T` suffix means typed with `Req` being `ReqT[_]`.
 */
sealed abstract class ReqT[+RT <: ReqTypeId] {
  val id: ReqIdT[RT]
  val pubid: PubidT[RT]
  val title: Text.AnyOptional

  def liveExplicitly: Live

  def live(reqTypes: ReqTypes): Live

  /** Can this req's (explicit-) live state be changed? */
  def allowLiveChange(reqTypes: ReqTypes): Permission

  @inline final def reqTypeId: RT =
    pubid.reqTypeId

  final def pastPubids(pr: PubidRegister): Set[Pubid] =
    pr.all(id) - pubid
}

object ReqT extends ReqTEquality {
  object IdAccess extends ObjDataId[ReqT.type, Req, ReqId] {
    override def id(d: Req) = d.id
    override val unapplyData: AnyRef => Option[Req] = {case r: Req => Some(r); case _ => None}
  }
}

// =====================================================================================================================
// Generic Req

final case class GenericReqId(value: Int) extends ReqIdT[CustomReqTypeId] {
  override def foldReqId[A](gr: GenericReqId => A, uc: UseCaseId => A): A = gr(this)
}

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

  def implicitLiveStatus(reqTypes: ReqTypes): ImplicitLiveStatus =
    reqTypes.need(pubid.reqTypeId).live match {
      case Live => ImplicitLiveStatus.NoImpact
      case Dead => ImplicitLiveStatus.ReqTypeIsDead
    }

  override def live(reqTypes: ReqTypes): Live =
    liveExplicitly & implicitLiveStatus(reqTypes).live

  override def allowLiveChange(reqTypes: ReqTypes): Permission =
    implicitLiveStatus(reqTypes) match {
      case ImplicitLiveStatus.NoImpact      => Allow
      case ImplicitLiveStatus.ReqTypeIsDead => Deny
    }
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

final case class UseCaseId(value: Int) extends ReqIdT[StaticReqType.UseCase] {
  override def foldReqId[A](gr: GenericReqId => A, uc: UseCaseId => A): A = uc(this)
}

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

  override def live(reqTypes: ReqTypes): Live =
    liveUC

  override def allowLiveChange(reqTypes: ReqTypes): Permission =
    Allow

  // Every use case has a mandatory UC-N.0 step
  def rootStep: UseCaseStep =
    stepsNA.tree.children.head.value

  val rootStepId: UseCaseStepId =
    rootStep.id

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

final case class UseCaseStepId(value: Int) extends SubReqId

@Lenses
final case class UseCaseStep(id             : UseCaseStepId,
                             titleExplicitly: Text.UseCaseStep.OptionalText,
                             liveExplicitly : Live) {

  def usesUseCaseTitle(enclosingUC: UseCase): Boolean =
    titleExplicitly.isEmpty && enclosingUC.rootStepId ==* id

  def title(enclosingUC: UseCase): UseCaseStep.Title =
    if (usesUseCaseTitle(enclosingUC))
      -\/(enclosingUC.title)
    else
      \/-(titleExplicitly)

  def titleA(enclosingUC: UseCase): Text.AnyOptional =
    if (usesUseCaseTitle(enclosingUC))
      enclosingUC.title
    else
      titleExplicitly

  @deprecated("Use UseCaseStep.live or UseCaseStep.Focus#live.", "")
  def live(a: Nothing): Nothing = a

  /** Doesn't take live-state of enclosing use-case into consideration. */
  def liveIgnoringUC(enclosingTree: UseCaseSteps): Live =
    liveIgnoringUC(enclosingTree.stepPartialLocs.get(id))

  /** Doesn't take live-state of enclosing use-case into consideration. */
  def liveIgnoringUC(ploc: VectorTree.PartialLocation): Live =
    UseCaseStep.liveIgnoringUC(ploc)
}

object UseCaseStep {
  object IdAccess extends ObjDataId[UseCaseStep.type, UseCaseStep, UseCaseStepId] {
    override def id(d: UseCaseStep) = d.id
    override val unapplyData: AnyRef => Option[UseCaseStep] = {case r: UseCaseStep => Some(r); case _ => None}
  }
  implicit def equality: UnivEq[UseCaseStep] = UnivEq.derive

  type Title = Text.UseCaseTitle.OptionalText \/ Text.UseCaseStep.OptionalText

  /** Live-state of a step. */
  def live(uc: UseCase, ploc: => VectorTree.PartialLocation): Live =
    uc.liveUC & liveIgnoringUC(ploc)

  /** Doesn't take live-state of enclosing use-case into consideration. */
  def liveIgnoringUC(ploc: VectorTree.PartialLocation): Live =
    Live.whenValid(ploc.validity)

  /**
   * Focus on a particular [[UseCaseStep]] and provide related data.
   */
  final class Focus(useCases: UseCases, val id: UseCaseStepId) { self =>

    val key: UseCases.StepTreeKey =
      useCases.stepIndex(id)

    @inline def field: StaticField.UseCaseStepTree =
      key.field

    @inline def useCaseId: UseCaseId =
      key.useCaseId

    val uc: UseCase =
      useCases.imap.need(key.useCaseId)

    val ucSteps: UseCaseSteps =
      key.field.useCaseSteps.get(uc)

    lazy val loc: VectorTree.Location =
      ucSteps.stepLocs.forward(id)

    val ploc: VectorTree.PartialLocation =
      ucSteps.stepPartialLocs.get(id)

    lazy val subtree: VectorTree.Node[UseCaseStep] =
      ucSteps.tree.needAt(loc)

    val step: UseCaseStep =
      useCases.needStep(id)

    def label(fmt: UseCaseStepLabelFmt): String =
      field.stepLabel(uc.pubid.pos, ploc, fmt)

    val live: Live =
      UseCaseStep.live(uc, ploc)

    def title: UseCaseStep.Title =
      step.title(uc)

    def titleA: Text.AnyOptional =
      step.titleA(uc)

    def textAndFlow(fd: FilterDead): UseCaseStepFlowText.TextAndFlow[Text.AnyOptional, Set[UseCaseStepId]] =
      UseCaseStepFlowText.TextAndFlow(titleA, Direction.Values(flow(_, fd)))

    def usesUseCaseTitle: Boolean =
      step.usesUseCaseTitle(uc)

    val canInsertAfterSelf: Permission =
      field.canInsertAfter(loc)

    val canShift: LeftRight => Permission = {
      lazy val canShiftRight = field.canShiftRight(loc, ucSteps.locValidity, ucSteps.tree.maxDepthTree);
      {
        case LeftRight.Right => canShiftRight
        case LeftRight.Left  => field.canShiftLeft(loc)
      }
    }

    def flow(d: Direction): Set[UseCaseStepId] =
      useCases.stepFlow(d)(id)

    def flow(d: Direction, fd: FilterDead): Set[UseCaseStepId] =
      fd match {
        case HideDead => flow(d, Live)
        case ShowDead => flow(d)
      }

    def flow(d: Direction, live: Live): Set[UseCaseStepId] =
      flow(d).filter(self.useCases.focusStep(_).live is live)
  }
}

/**
 * A tree of steps. Can correspond to one (EC) or more (NC + AC) fields.
 */
@Lenses
final case class UseCaseSteps(tree: UseCaseSteps.Tree) {
  import VectorTree.{Location, PartialLocation}

  def need(id: UseCaseStepId): UseCaseStep =
    tree.needAtLocation(stepLocs.forward(id))

  lazy val stepLocs: BiMap[UseCaseStepId, Location] = {
    val b = BiMap.newBuilder[UseCaseStepId, Location]
    tree.foreach((l, s) => b.update(s.id, l))
    b.result()
  }

  lazy val partialLocs: BiMap[Location, PartialLocation] =
    BiMap(tree.partLocs(step =>
      step.liveExplicitly match {
        case Live => VectorTree.NodeFilter.KeepNode
        case Dead => VectorTree.NodeFilter.DiscardNodeAndChildren
      }
    ))

  val locValidity: Location => Validity =
    l => partialLocs.forward(l).validity

  lazy val stepPartialLocs: Iso[UseCaseStepId, PartialLocation] =
    Optics.biMapIso_!(stepLocs) ^<-> Optics.biMapIso_!(partialLocs)

  lazy val partialLocSteps: Intersection[PartialLocation, UseCaseStepId] =
    (Intersection.fromBiMap(stepLocs) <=> Intersection.fromBiMap(partialLocs)).reverse
}

object UseCaseSteps {
  type Tree = VectorTree[UseCaseStep]
  implicit def equality: UnivEq[UseCaseSteps] = UnivEq.derive

  val empty: UseCaseSteps =
    UseCaseSteps(VectorTree.empty)

  def emptyRoot(id: UseCaseStepId): UseCaseSteps =
    single(UseCaseStep(id, Text.UseCaseStep.empty, Live))

  def single(s: UseCaseStep): UseCaseSteps =
    UseCaseSteps(VectorTree single s)
}

/**
 * @param stepIndex An index of all [[UseCaseStep]]s and the static portions of their locations.
 *                  This is calculable state which is normally never manually-managed, but is in this case due to the
 *                  frequency of step lookup and the ease of maintaining it (only two events affect it).
 * @param stepFlow Explicitly declared flow between steps.
 *                 Note that the position of steps provides implicit flow that isn't stored here (or anywhere).
 */
@Lenses
final case class UseCases(imap: UseCaseIMap, stepIndex: UseCases.StepIndex, stepFlow: UseCases.StepFlow) {

  @inline def need(id: UseCaseId): UseCase =
    imap.need(id)

  def stepIterator: Iterator[UseCaseStep] =
    imap.valuesIterator.flatMap(_.stepIterator)

  def stepIdSet: Set[UseCaseStepId] =
    stepIterator.map(_.id).toSet

  def focusStepIterator(): Iterator[UseCaseStep.Focus] =
    stepIndex.keysIterator.map(focusStep)

  def liveStepIterator(): Iterator[UseCaseStep.Focus] =
    focusStepIterator().filter(_.live is Live)

  val focusStep: UseCaseStepId => UseCaseStep.Focus =
    Memo(new UseCaseStep.Focus(this, _))

  def getFocusStep(id: UseCaseStepId): Option[UseCaseStep.Focus] =
    Option.when(stepIndex.contains(id))(focusStep(id))

  def getStep(id: UseCaseStepId): Option[UseCaseStep] =
    stepIndex.get(id).map(_.need(imap).need(id))

  def needStep(id: UseCaseStepId): UseCaseStep =
    stepIndex(id).need(imap).need(id)
}

object UseCases {

  /**
   * Information sufficient to uniquely identify a step tree within a project.
   */
  final case class StepTreeKey(useCaseId: UseCaseId, field: StaticField.UseCaseStepTree) {
    def need(imap: UseCaseIMap): UseCaseSteps =
      field.useCaseSteps.get(imap.need(useCaseId))
  }

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
      uc <- imap.valuesIterator
      id = uc.id
      f  <- StaticField.useCaseStepTrees
      s  <- f.useCaseStepTree.get(uc).valueIterator
    } m = m.updated(s.id, StepTreeKey(id, f))
    m
  }

  val StepFlow = new Digraph.Fix[UseCaseStepId]
  type StepFlow = StepFlow.BiDir

  implicit lazy val equality: Equal[UseCases] =
    ScalazMacros.deriveEqual

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
    ScalazMacros.deriveEqual

  def empty: Requirements =
    Requirements(emptyDataMap(GenericReq), UseCases.empty, PubidRegister.empty)
}

@Lenses
final case class Requirements(genericReqs: GenericReqIMap,
                              useCases   : UseCases,
                              pubids     : PubidRegister) {

  def isEmpty = reqIterator().isEmpty
  def nonEmpty = !isEmpty

  def idIterator(): Iterator[ReqId] =
    reqIterator().map(_.id)

  val all: View[Req] =
    View.fromIteratorProvider(() => reqIterator())

  def reqIterator(): Iterator[Req] =
    genericReqs.valuesIterator ++
    useCases.imap.valuesIterator

  lazy val size: Int =
    genericReqs.size + useCases.imap.size

  def getUseCaseByPos(pos: ReqTypePos): Option[UseCase] =
    pubids.getUseCaseId(pos) flatMap useCases.imap.get

  def get[T <: ReqTypeId](id: ReqIdT[T]): Option[ReqT[T]] =
    id match {
      case i: GenericReqId => genericReqs.get(i)
      case i: UseCaseId    => useCases.imap.get(i)
    }

  def need[T <: ReqTypeId](id: ReqIdT[T]): ReqT[T] =
    id match {
      case i: GenericReqId => genericReqs.need(i)
      case i: UseCaseId    => useCases.imap.need(i)
    }

  def getReqByPubid[T <: ReqTypeId](id: PubidT[T]): Option[ReqT[T]] =
    pubids(id) flatMap get

  def needByPubid[T <: ReqTypeId](id: PubidT[T]): ReqT[T] =
    getReqByPubid(id) mustExistElse s"Req for $id not found."

  def reqIdByPubid[T <: ReqTypeId](id: PubidT[T]): ReqIdT[T] =
    pubids(id) mustExistElse s"Req for $id not found."

  lazy val reqsByType: Multimap[ReqTypeId, Vector, Req] =
    reqIterator().foldLeft(UnivEq.emptyMultimap[ReqTypeId, Vector, Req])((q, r) =>
      q.add(r.reqTypeId, r))

  lazy val useCaseStepLabelLookup: UseCaseStepLabelLookup =
    new UseCaseStepLabelLookup(this)

  /** For a given req type, returns the set of requirements that used to be associated with the given req type but
   * no longer are primarily (hence the term "ex-req").
   */
  val exReqs: ReqTypeId => Set[ReqId] =
    Memo {
      case _: StaticReqType =>
        Set.empty

      case id: CustomReqTypeId =>
        pubids.value(id)
          .iterator
          .filter(need(_).reqTypeId !=* id)
          .toSet
    }
}