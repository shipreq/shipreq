package shipreq.webapp.member.project.data

import cats.Eq
import japgolly.microlibs.cats_ext.CatsMacros
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import monocle.macros.Lenses
import monocle.std.option.pSome
import monocle.{Lens, Traversal}
import shipreq.base.util._
import shipreq.webapp.base.data.{ProjectCreator, UserId}
import shipreq.webapp.member.project.data.derivation._
import shipreq.webapp.member.project.event.{ApplyEvent, Event, EventOrd, ProjectEvents, VerifiedEvent}
import shipreq.webapp.member.project.issue.IssueTracker
import shipreq.webapp.member.project.text.PlainText

object Project {
  type Name = String

  val customIssueTypes    : Lens[Project, CustomIssueTypeIMap      ] = config  andThen ProjectConfig.customIssueTypes
  val reqTypes            : Lens[Project, ReqTypes                 ] = config  andThen ProjectConfig.reqTypes
  val fields              : Lens[Project, FieldSet                 ] = config  andThen ProjectConfig.fields
  val tagTree             : Lens[Project, TagTree                  ] = config  andThen ProjectConfig.tags andThen Tags.tree
  val customFields        : Lens[Project, FieldSet.CustomFields    ] = fields  andThen FieldSet.customFields
  val reqs                : Lens[Project, Requirements             ] = content andThen ProjectContent.reqs
  val reqCodes            : Lens[Project, ReqCodes                 ] = content andThen ProjectContent.reqCodes
  val reqText             : Lens[Project, ReqData.Text             ] = content andThen ProjectContent.reqText
  val reqTags             : Lens[Project, ReqData.Tags             ] = content andThen ProjectContent.reqTags
  val implications        : Lens[Project, Implications.Graph.BiDir ] = content andThen ProjectContent.implications andThen Implications.graph
  val deletionReasons     : Lens[Project, DeletionReasons          ] = content andThen ProjectContent.deletionReasons
  val genericReqs         : Lens[Project, GenericReqIMap           ] = content andThen ProjectContent.genericReqs
  val useCases            : Lens[Project, UseCases                 ] = content andThen ProjectContent.useCases
  val pubidRegister       : Lens[Project, PubidRegister            ] = content andThen ProjectContent.pubidRegister
  val reqCodeTrie         : Lens[Project, ReqCode.Trie             ] = content andThen ProjectContent.reqCodeTrie
  val implicationsSrcToTgt: Lens[Project, Implications.Graph.UniDir] = content andThen ProjectContent.implicationsSrcToTgt
  val useCaseIMap         : Lens[Project, UseCaseIMap              ] = content andThen ProjectContent.useCaseIMap
  val useCaseStepIndex    : Lens[Project, UseCases.StepIndex       ] = content andThen ProjectContent.useCaseStepIndex

  val applicableTags: Traversal[Project, ApplicableTag] =
    tagTree andThen TagTree.traversal andThen TagInTree.tag andThen Tag.applicableTag

  val savedViewsNE: monocle.Optional[Project, savedview.SavedViews.NonEmpty] = {
    import savedview.SavedViews.NonEmpty
    savedViews andThen pSome[NonEmpty, NonEmpty]
  }

  def savedView(id: savedview.SavedView.Id): monocle.Optional[Project, savedview.SavedView] =
    savedViewsNE andThen savedview.SavedViews.NonEmpty.at(id)

  val savedViewTraversal: Traversal[Project, savedview.View] =
    savedViewsNE andThen savedview.SavedViews.NonEmpty.traversalSavedView andThen savedview.SavedView.view

  // Not allowed by validator.
  // This ensures that initial ProjectNameSet events (generated on project creation) apply instead of being discarded
  // as NO-OPs due to the hashcodes being unchanged before and after.
  final val emptyProjectName: Name =
    ""

  def init(creator: ProjectCreator): Project =
    _init(ProjectAccess.init(creator))

  val empty: Project =
    _init(ProjectAccess.empty)

  private def _init(access: ProjectAccess): Project =
    Project(
      name         = emptyProjectName,
      config       = ProjectConfig.empty,
      content      = ProjectContent.empty,
      manualIssues = ManualIssues.empty,
      savedViews   = savedview.SavedViews.empty,
      access       = access,
      history      = ProjectEvents.empty,
      idCeilings   = IdCeilings.zero,
    )

  def reqIdsSortedByPubId(reqs: Requirements, reqTypes: ReqTypes): ArraySeq[ReqId] =
    reqTypes.allSortedByMnemonic
      .iterator
      .flatMap(rt => reqs.pubids.value(rt.reqTypeId))
      .to(ArraySeq)

  // -------------------------------------------------------------------------------------------------------------------

  final class Equality(implicit val equalProjectEvents: Eq[ProjectEvents]) {
    implicit val equalProject: Eq[Project] =
      CatsMacros.deriveEq
  }

  object Equality {
    lazy val WithHistoryByOrd: Equality = {
      import ProjectEvents.ImplicitEqualityByOrd._
      new Equality
    }

    lazy val IgnoringHistory: Equality = {
      implicit val equalProjectEvents: Eq[ProjectEvents] =
        (_, _) => true
      new Equality
    }
  }

  implicit val canCmp: EventOrd.CanCmp[Project] =
    EventOrd.CanCmp(_.ordAsInt)

  implicit val canCmpOption: EventOrd.CanCmp[Option[Project]] =
    canCmp.option
}

// =====================================================================================================================

@Lenses
final case class Project(name        : Project.Name,
                         config      : ProjectConfig,
                         content     : ProjectContent,
                         manualIssues: ManualIssues,
                         savedViews  : savedview.SavedViews.Optional,
                         access      : ProjectAccess,
                         history     : ProjectEvents,
                         idCeilings  : IdCeilings) extends EventOrd.CmpOps {

  override def toString =
    s"Project{v${history.ordAsInt}}"
    //ShowSize(this).showTree

  @inline def update(ves: VerifiedEvent.NonEmptySeq): ErrorMsg \/ Project =
    update(ves.values)

  @inline def update(ves: VerifiedEvent.Seq): ErrorMsg \/ Project =
    ApplyEvent.trusted(ves)(this)

  @inline def update(ve: VerifiedEvent): ErrorMsg \/ Project =
    ApplyEvent.trusted(ve)(this)

  private def _updateOrThrow(r: ApplyEvent.Result): Project =
    r.fold(_.withPrefix("Project update failed. ").throwException(), identity)

  def updateOrThrow(ves: VerifiedEvent.NonEmptySeq): Project =
    _updateOrThrow(update(ves))

  def updateOrThrow(ves: VerifiedEvent.Seq): Project =
    _updateOrThrow(update(ves))

  def updateOrThrow(ve: VerifiedEvent): Project =
    _updateOrThrow(update(ve))

  @inline def ord =
    history.ord

  override def ordAsInt =
    history.ordAsInt

  def min(p: Project): Project = if (this < p) this else p
  def max(p: Project): Project = if (this > p) this else p

  def min(p: Option[Project]): Project = p.fold(this)(min)
  def max(p: Option[Project]): Project = p.fold(this)(max)

  lazy val deadReqIds: Set[ReqId] =
    content.reqs.reqIterator().filter(_.live(config.reqTypes) is Dead).map(_.id).toSet

  lazy val deadReqCount: Int =
    deadReqIds.size

  lazy val reqTypeCount: LiveDeadStatMap[ReqTypeId, Int] = {
    val b = LiveDeadStatMap.Builder.ofInts[ReqTypeId]()

    // Add reqs
    for (r <- content.reqs.reqIterator()) {
      val live = r.live(config.reqTypes)
      b(r.reqTypeId).add(live, 1)
    }

    // Add ex-reqs
    for (reqTypeId <- config.reqTypes.custom.keys) {
      val exs = content.reqs.exReqs(reqTypeId)
      if (exs.nonEmpty)
        b(reqTypeId).add(Dead, exs.size)
    }

    b.result()
  }

  lazy val atomScan = AtomScan(this)

  lazy val dataLogic = new DataLogic(this)

  lazy val issues = IssueTracker(this).issues

  lazy val virtualTags = VirtualProjectTags(this)

  def deletionMethodForUseCaseStep(id: UseCaseStepId): DeletionMethod =
    DeletionMethod.Hard.unless {
      val f           = content.reqs.useCases.focusStep(id)
      def hasTitle    = f.step.titleExplicitly.nonEmpty
      def childNeeded = f.subtree.children.exists(n => deletionMethodForUseCaseStep(n.value.id) is DeletionMethod.Soft)
      def hasFlow     = Direction.exists(content.reqs.useCases.stepFlow(_).valuesIterator.exists(_.contains(id)))
      def refdInText  = content.useCaseStepRefs.contains(id)
      hasTitle || childNeeded || hasFlow || refdInText
    }

  /**
   * Transitive closure of implications going source -> target.
   *
   * Note: Dead reqs are included (reflexively and when direct implications) but are not followed.
   */
  lazy val implicationSrcToTgtTC: TransitiveClosure[ReqId] =
    implicationTransitiveClosure(Forwards)

  /**
   * Transitive closure of implications going target -> source.
   *
   * Note: Dead reqs are included (reflexively and when direct implications) but are not followed.
   */
  lazy val implicationTgtToSrcTC: TransitiveClosure[ReqId] =
    implicationTransitiveClosure(Backwards)

  private def implicationTransitiveClosure(dir: Direction): TransitiveClosure[ReqId] =
    content.implications.graph.transitiveClosure(
      dir,
      content.reqs.idIterator(),
      TransitiveClosure.Filter terminalSet deadReqIds)

  def reqIterator(filterDead: FilterDead): Iterator[Req] =
    filterDead match {
      case HideDead => liveReqIterator()
      case ShowDead => content.reqs.all.iterator
    }

  def liveReqIterator(): Iterator[Req] =
    content.reqs.reqIterator().filter(_.live(config.reqTypes) is Live)

  lazy val liveReqCount: Int =
    liveReqIterator().size

  def savedViewIterator: Iterator[savedview.SavedView] =
    savedViews.fold[Iterator[savedview.SavedView]](Iterator.empty)(_.iterator)

  def isReqTypeInUse(id: CustomReqTypeId): Boolean =
    content.reqs.pubids.value(id).nonEmpty

  def naTagsForReq(reqId: Option[ReqId]): NaTags =
    reqId.fold(NaTags.none)(naTagsForReq(_))

  def naTagsForReq(reqId: ReqId): NaTags = {
    val req = content.reqs.need(reqId)
    naTagsForReq(req)
  }

  @inline def naTagsForReq(req: Req): NaTags =
    config.naTags(req.reqTypeId)

  @elidable(elidable.FINEST)
  def prettyPrintImplicationGraph: String =
    Util.quickJSB { sb =>

      val indentFn: Int => String =
        Memo(". " * _)

      val _fmt: ReqId => String =
        id => {
          val req = content.reqs.need(id)
          var a = s"${PlainText.pubidByReqId(id, this)} (#${id.value})"
          if (req.live(config.reqTypes) is Dead) {
            a += (if (req.liveExplicitly is Dead) " [DEAD EXP]" else " [DEAD IMP]")
            if (req.allowLiveChange(config.reqTypes) is Deny) a += " [!RESTORE]"
          }
          a
        }

      val fmt = Memo(_fmt)

      var first = true

      def go(_ids: IterableOnce[ReqId], indent: Int): Unit = {
        val indentStr = indentFn(indent)
        MutableArray(_ids).map(id => (id, fmt(id))).sortBy(_._2).array.foreach { case (id, show) =>
          if (first) first = false else sb append '\n'
          sb append indentStr
          sb append show
          go(content.implications.forwards(id), indent + 1)
        }
      }
      go(content.reqs.idIterator().filter(content.implications.backwards(_).isEmpty), 0)
    }

  lazy val allKnownUsers: Set[UserId] =
    access.asMap.keySet ++
      // This covers all VerifiedEvent authors
      history.events.iterator.map(_.event).flatMap {
        case e: Event.AccessUpdate => Some(e.userId)
        case _                     => None
      }
}
