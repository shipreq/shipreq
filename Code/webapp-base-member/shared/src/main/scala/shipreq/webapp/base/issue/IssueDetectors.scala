package shipreq.webapp.base.issue

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptySet
import shipreq.base.util.Util
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Atom

object IssueDetectors {
  import IssueDetector.{Increment, Init}

  sealed trait Instance extends IssueDetector {
    protected def redoAllIf(i: Increment, redo: Boolean): Unit =
      if (redo) {
        i.invalidateAll()
        init(i.init)
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object BlankTitle extends Instance {

    override def init(i: Init): Unit =
      i.action.foreachDirtyLiveReq(() => detectInReqs(i))

    override def increment(i: Increment): Unit =
      init(i.init)

    private def detectInReqs(i: Init): Req => Unit =
      req =>
        if (req.title.isEmpty)
          i.action.add(Issue.BlankTitle(req.id))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object ConflictingTags extends Instance {

    override def init(i: Init): Unit =
      i.action.foreachDirtyLiveReq(() => detectInReqs(i))

    override def increment(i: Increment): Unit = {
      if (i.eventSummary.hasTags || i.eventSummary.customFieldTextTypes.hasDR)
        i.invalidateAll()
      init(i.init)
    }

    private def detectInReqs(i: Init): Req => Unit = {
      val exclusiveGroups = i.project.config.tags.exclusiveGroups
      val tagLookup       = i.project.dataLogic.tagLookup(HideDead)
      req => {
        val reqId     = req.id
        val tagIds    = tagLookup(reqId).other
        val conflicts = Util.uniqueDupsNested(tagIds.keyIterator)(exclusiveGroups)
        for (g <- conflicts) {
          val locs: Set[ReqTagLoc] =
            tagIds.iterator
              .filter(x => exclusiveGroups(x._1).contains(g))
              .flatMap(_._2)
              .toSet
          i.action.add(Issue.ConflictingTags(reqId, g, NonEmptySet force locs))
        }
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object DeadReference extends Instance {

    override def init(i: Init): Unit = {
      i.action.foreachDirtyLiveReq(() => detectInReqs(i))
      i.action.foreachDirtyLiveRcg(() => detectInRcgs(i))
    }

    override def increment(i: Increment): Unit = {
      val s = i.eventSummary
      if ( s.contentLiveDeps
        || s.genericReqs.hasDR
        || s.useCasesExclSteps.hasDR
        || s.useCaseSteps.hasDR
        || s.reqCodeGroups.hasDR
        || s.customFieldTextTypes.hasDR
        || s.apReqCodes
      )
        i.invalidateAll()
      init(i.init)
    }

    private def detectInReqs(i: Init): Req => Unit = {
      val refsInReqs = i.project.atomScan.contentRefsInReqs
      req =>
        for (a <- refsInReqs(req.id).live)
          if (!isRefLive(a.value, i.project))
            i.action.add(Issue.DeadRefInReq(req.id, a.loc, ContentRef.fromAtom(a.value)))
    }

    private def detectInRcgs(i: Init): LiveCodeGroup => Unit = {
      val refsInRcgs = i.project.atomScan.contentRefsInRcgs
      rcg =>
        for (a <- refsInRcgs(rcg.id).live)
          if (!isRefLive(a, i.project))
            i.action.add(Issue.DeadRefInRcg(rcg.id, ContentRef.fromAtom(a)))
    }

    private def isRefLive(a: Atom.AnyContentRef, p: Project): Boolean =
      a match {
        case r: Atom.ContentRef # ReqRef         => p.content.reqs.need(r.value).live(p.config.reqTypes).is(Live)
        case r: Atom.ContentRef # CodeRef        => p.content.reqCodes.needById(r.value).isActive
        case r: Atom.ContentRef # UseCaseStepRef => p.content.reqs.useCases.focusStep(r.value).live.is(Live)
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object DeadTag extends Instance {

    override def init(i: Init): Unit =
      i.action.foreachDirtyLiveReq(() => detectInReqs(i))

    override def increment(i: Increment): Unit = {
      if (i.eventSummary.hasTagsDR || i.eventSummary.customFieldTextTypes.hasDR)
        i.invalidateAll()
      init(i.init)
    }

    private def detectInReqs(i: Init): Req => Unit = {
      val tagRefs = i.project.atomScan.tagRefs
      req => {
        for (a <- tagRefs(req.id).live) {
          val t = i.project.config.tags.atag(a.value)
          if (t.live.is(Dead))
            i.action.add(Issue.DeadTag(req.id, a.loc, a.value))
        }
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object EmptyCodeGroup extends Instance {

    override def init(i: Init): Unit =
      i.action.foreachDirtyLiveRcg(() => detectInRcgs(i))

    override def increment(i: Increment): Unit =
      init(i.init)

    private def detectInRcgs(i: Init): LiveCodeGroup => Unit = {
      val reqCodes = i.project.content.reqCodes
      rcg => {
        val code    = reqCodes.reqCodeGroupsById(rcg.id)
        val subtree = reqCodes.trie.getNode(code).get
        val empty   = subtree.valueIterator().forall(isEmpty)
        if (empty)
          i.action.add(Issue.EmptyCodeGroup(rcg.id))
      }
    }

    private val isEmpty: ReqCode.Data => Boolean = {
      case _: ReqCode.ActiveReq   => false
      case _: ReqCode.ActiveGroup
         | _: ReqCode.Inactive    => true
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object IssueTags extends Instance {

    override def init(i: Init): Unit = {
      i.action.foreachDirtyLiveReq(() => detectInReqs(i))
      i.action.foreachDirtyLiveRcg(() => detectInRcgs(i))
    }

    override def increment(i: Increment): Unit = {
      if (i.eventSummary.customIssueTypes.hasDR || i.eventSummary.customFieldTextTypes.hasDR)
        i.invalidateAll()
      init(i.init)
    }

    private def detectInReqs(i: Init): Req => Unit = {
      val issuesInReqs = i.project.atomScan.issuesInReqs
      req => {
        for (a <- issuesInReqs(req.id).live) {
          val t = i.project.config.customIssueType(a.value.typ)
          val r = t.live match {
            case Live => Issue.IssueTagInReq(req.id, a.loc, a.value)
            case Dead => Issue.DeadIssueTagInReq(req.id, a.loc, a.value)
          }
          i.action.add(r)
        }
      }
    }

    private def detectInRcgs(i: Init): LiveCodeGroup => Unit = {
      val issuesInRcgs = i.project.atomScan.issuesInRcgs
      rcg => {
        for (a <- issuesInRcgs(rcg.id).live) {
          val t = i.project.config.customIssueType(a.typ)
          val r = t.live match {
            case Live => Issue.IssueTagInRcg(rcg.id, a)
            case Dead => Issue.DeadIssueTagInRcg(rcg.id, a)
          }
          i.action.add(r)
        }
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object LooseIssue extends Instance {

    override def init(i: Init): Unit =
      ()

    override def increment(i: Increment): Unit =
      ()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object UninhabitableTagField extends Instance {

    override def init(i: Init): Unit = {
      val cfg = i.project.config
      for (f <- i.project.config.fields.customTagFields) {
        val isLive        = f.liveExplicitly is Live
        def uninhabitable = !inhabitable(f.tagId, cfg)
        if (isLive && uninhabitable)
          i.action.add(Issue.UninhabitableTagField(f.id))
      }
    }

    override def increment(i: Increment): Unit =
      redoAllIf(i, i.eventSummary.hasTags || i.eventSummary.customFieldTagTypes.hasAny)

    private def inhabitable(id: TagId, cfg: ProjectConfig): Boolean = {
      val t      = cfg.tags.tree.need(id)
      val isLive = t.tag.live is Live
      def typeOk = t.tag match {
        case _: ApplicableTag => true
        case _: TagGroup      => t.children.exists(inhabitable(_, cfg))

      }
      isLive && typeOk
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val all = AdtMacros.adtValues[Instance]
}
