package shipreq.webapp.base.data

import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import monocle.{Lens, Traversal}
import monocle.macros.Lenses
import monocle.std.option.pSome
import nyaya.util.Multimap
import scala.collection.compat.immutable.ArraySeq
import scalaz.Equal
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.issue.{Issue, IssueTracker}

object Project {
  type Name = String

  val customIssueTypes    : Lens[Project, CustomIssueTypeIMap  ] = config  ^|-> ProjectConfig.customIssueTypes
  val reqTypes            : Lens[Project, ReqTypes             ] = config  ^|-> ProjectConfig.reqTypes
  val fields              : Lens[Project, FieldSet             ] = config  ^|-> ProjectConfig.fields
  val tagTree             : Lens[Project, TagTree              ] = config  ^|-> ProjectConfig.tags ^|-> Tags.tree
  val customFields        : Lens[Project, FieldSet.CustomFields] = fields  ^|-> FieldSet.customFields
  val reqs                : Lens[Project, Requirements         ] = content ^|-> ProjectContent.reqs
  val reqCodes            : Lens[Project, ReqCodes             ] = content ^|-> ProjectContent.reqCodes
  val reqText             : Lens[Project, ReqData.Text         ] = content ^|-> ProjectContent.reqText
  val reqTags             : Lens[Project, ReqData.Tags         ] = content ^|-> ProjectContent.reqTags
  val implications        : Lens[Project, Implications.BiDir   ] = content ^|-> ProjectContent.implications
  val deletionReasons     : Lens[Project, DeletionReasons      ] = content ^|-> ProjectContent.deletionReasons
  val genericReqs         : Lens[Project, GenericReqIMap       ] = content ^|-> ProjectContent.genericReqs
  val useCases            : Lens[Project, UseCases             ] = content ^|-> ProjectContent.useCases
  val pubidRegister       : Lens[Project, PubidRegister        ] = content ^|-> ProjectContent.pubidRegister
  val reqCodeTrie         : Lens[Project, ReqCode.Trie         ] = content ^|-> ProjectContent.reqCodeTrie
  val implicationsSrcToTgt: Lens[Project, Implications.UniDir  ] = content ^|-> ProjectContent.implicationsSrcToTgt
  val useCaseIMap         : Lens[Project, UseCaseIMap          ] = content ^|-> ProjectContent.useCaseIMap
  val useCaseStepIndex    : Lens[Project, UseCases.StepIndex   ] = content ^|-> ProjectContent.useCaseStepIndex

  val applicableTags: Traversal[Project, ApplicableTag] =
    tagTree ^|->> TagTree.traversal ^|-> TagInTree.tag ^|-? Tag.applicableTag

  val savedViewsNE: monocle.Optional[Project, savedview.SavedViews.NonEmpty] =
    savedViews ^<-? pSome

  def savedView(id: savedview.SavedView.Id): monocle.Optional[Project, savedview.SavedView] =
    savedViewsNE ^|-? savedview.SavedViews.NonEmpty.at(id)

  val savedViewTraversal: Traversal[Project, savedview.View] =
    savedViewsNE ^|->> savedview.SavedViews.NonEmpty.traversalSavedView ^|-> savedview.SavedView.view

  implicit lazy val equality: Equal[Project] =
    ScalazMacros.deriveEqual

  // Not allowed by validator.
  // This ensures that initial ProjectNameSet events (generated on project creation) apply instead of being discarded
  // as NO-OPs due to the hashcodes being unchanged before and after.
  final val emptyProjectName: Name =
    ""

  val empty: Project =
    Project(
      name         = emptyProjectName,
      config       = ProjectConfig.empty,
      content      = ProjectContent.empty,
      manualIssues = ManualIssues.empty,
      savedViews   = savedview.SavedViews.empty,
      idCeilings   = IdCeilings.zero)

  def reqIdsSortedByPubId(reqs: Requirements, reqTypes: ReqTypes): ArraySeq[ReqId] =
    reqTypes.allSortedByMnemonic
      .iterator
      .flatMap(rt => reqs.pubids.value(rt.reqTypeId))
      .to(ArraySeq)
}

@Lenses
final case class Project(name        : Project.Name,
                         config      : ProjectConfig,
                         content     : ProjectContent,
                         manualIssues: ManualIssues,
                         savedViews  : savedview.SavedViews.Optional,
                         idCeilings  : IdCeilings) {

  override def toString =
    s"Project($idCeilings)"
    //ShowSize(this).showTree

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

  def reqTagsFn(tagGroupId: TagGroupId, filterDead: FilterDead): ReqId => Vector[ApplicableTagId] = {
    val tagLookup = dataLogic.tagLookup(filterDead)
    val tagScope  = config.tagFieldDistribution(filterDead).inTagGroup(tagGroupId)
    val tagOrder  = config.tags.applicableTagOrdering(tagGroupId, filterDead)

    reqId =>
      MutableArray(tagLookup(reqId).all.iterator.filter(tagScope.contains))
        .sort(tagOrder)
        .iterator()
        .toVector
  }

  def deletionMethodForUseCaseStep(id: UseCaseStepId): DeletionMethod =
    DeletionMethod.Hard.unless {
      val f           = content.reqs.useCases.focusStep(id)
      def hasTitle    = f.step.titleExplicitly.nonEmpty
      def childNeeded = f.subtree.children.exists(n => deletionMethodForUseCaseStep(n.value.id) is DeletionMethod.Soft)
      def hasFlow     = Direction.exists(content.reqs.useCases.stepFlow(_).valuesIterator.exists(_.contains(id)))
      def refdInText  = content.useCaseStepRefs.contains(id)
      hasTitle || childNeeded || hasFlow || refdInText
    }

  private lazy val conflictingTagsPerReq: Multimap[ReqId, Set, ApplicableTagId] = {
    var m = Multimap.empty[ReqId, Set, ApplicableTagId]
    issues.vector.foreach {

      case i: Issue.ConflictingTags =>
        val tagGroup = config.tags.tree.need(i.tagGroupId)
        val children = tagGroup.transitiveChildren(config.tags.tree)
        val atags    = children.iterator.filterSubType[ApplicableTagId].toSet
        m = m.addvs(i.req.id, atags)

      case _ => ()
    }
    m
  }

  lazy val invalidTagsPerReq: ReqId => Set[ApplicableTagId] =
    Memo { reqId =>
      val req           = content.reqs.need(reqId)
      val conflicting   = conflictingTagsPerReq(reqId)
      val nonApplicable = config.naTags(req.reqTypeId).set
      conflicting ++ nonApplicable
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
    content.implications.transitiveClosure(
      dir,
      content.reqs.idIterator(),
      TransitiveClosure.Filter terminalSet deadReqIds)

  def liveReqIterator(): Iterator[Req] =
    content.reqs.reqIterator().filter(_.live(config.reqTypes) is Live)

  lazy val liveReqCount: Int =
    liveReqIterator().size

  def savedViewIterator: Iterator[savedview.SavedView] =
    savedViews.fold[Iterator[savedview.SavedView]](Iterator.empty)(_.iterator)

  def fieldDefaultApplied(fieldId: CustomField.Tag.Id, filterDead: FilterDead): ReqId => Boolean = {
    val scope = config.tagFieldDistribution(filterDead).inField(fieldId)
    val tagLookup = dataLogic.tagLookup(filterDead)
    reqId => tagLookup(reqId).fieldDefaultApplied(scope)
  }

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

  def prettyPrintImplicationGraph: String =
    Util.quickJSB { sb =>

      val indentFn: Int => String =
        Memo(". " * _)

      val _fmt: ReqId => String =
        id => {
          var a = id.value.toString
          val r = content.reqs.need(id)
          if (r.live(config.reqTypes) is Dead) {
            a += (if (r.liveExplicitly is Dead) "!" else "-")
            if (r.allowLiveChange(config.reqTypes) is Deny) a += "!"
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
}
