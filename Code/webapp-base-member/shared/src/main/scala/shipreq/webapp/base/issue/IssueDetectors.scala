package shipreq.webapp.base.issue

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptySet
import shipreq.base.util.{Backwards, Util}
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{Atom, Text}

object IssueDetectors {
  import IssueDetector.Ctx

  sealed trait Instance extends IssueDetector

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object BlankCustomField extends Instance {

    override val detect = ctx => {

      // Find fields
      val fields = new CustomField.MutableLists
      val cfg = ctx.project.config
      for (f <- ctx.project.config.fields.customFields.valuesIterator)
        if (f.mandatory.is(Mandatory) && f.live(cfg).is(Live))
          fields += f

      // Check imps
      run(ctx, fields.imp) {
        val byField = ctx.project.dataLogic.customFieldImps(HideDead)
        f => {
          val imps = byField(f.id)
          imps(_).isEmpty
        }
      }

      // Check tags
      run(ctx, fields.tag) {
        val tagLookup = ctx.project.dataLogic.tagLookup(HideDead)
        val dist = ctx.project.config.liveTagFieldDistribution
        f => {
          val tags = DataLogic.customFieldTags(dist, tagLookup, f.id)
          tags(_).isEmpty
        }
      }

      // Check text
      run(ctx, fields.text) { f =>
        val textMap = ctx.project.content.reqTextFor(f.id)
        !textMap.contains(_)
      }
    }

    private def run[F <: CustomField, A](ctx: Ctx, fields: List[F])
                                        (prepare: => (F => ReqId => Boolean)): Unit =
      if (fields.nonEmpty)
        ctx.foreachLiveReq(() => {
          val p = prepare
          val as = fields.map(f => f -> p(f))
          req => {
            val reqId     = req.id
            val reqTypeId = req.reqTypeId
            for ((field, hasIssue) <- as)
              if (field.reqTypes.contains(reqTypeId) && hasIssue(reqId))
                ctx.add(Issue.BlankCustomField(reqId, field.id))
          }
        })
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object BlankTitle extends Instance {
    override val detect = ctx =>
      ctx.foreachLiveReq(() => req =>
        if (req.title.isEmpty)
          ctx.add(Issue.BlankTitle(req.id)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object BlankUseCaseStep extends Instance {
    override val detect = ctx =>
      ctx.foreachLiveUcs(() => f =>
        if (f.step.titleExplicitly.isEmpty && !f.usesUseCaseTitle)
          ctx.add(Issue.BlankUseCaseStep(f.useCaseId, f.id)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object ConflictingTags extends Instance {

    override val detect = ctx =>
      ctx.foreachLiveReq(() => detectInReqs(ctx))

    private def detectInReqs(ctx: Ctx): Req => Unit = {
      val exclusiveGroups = ctx.project.config.tags.exclusiveGroups
      val tagLookup       = ctx.project.dataLogic.tagLookup(HideDead)
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
          ctx.add(Issue.ConflictingTags(reqId, g, NonEmptySet force locs))
        }
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object DeadReference extends Instance {

    override val detect = ctx => {
      ctx.foreachLiveReq(() => detectInReqs(ctx))
      ctx.foreachLiveRcg(() => detectInRcgs(ctx))
    }

    private def detectInReqs(ctx: Ctx): Req => Unit = {
      val refsInReqs = ctx.project.atomScan.contentRefsInReqs
      req =>
        for (a <- refsInReqs(req.id).live)
          if (!isRefLive(a.value, ctx.project))
            ctx.add(Issue.DeadRefInReq(req.id, a.loc, ContentRef.fromAtom(a.value)))
    }

    private def detectInRcgs(ctx: Ctx): LiveCodeGroup => Unit = {
      val refsInRcgs = ctx.project.atomScan.contentRefsInRcgs
      rcg =>
        for (a <- refsInRcgs(rcg.id).live)
          if (!isRefLive(a, ctx.project))
            ctx.add(Issue.DeadRefInRcg(rcg.id, ContentRef.fromAtom(a)))
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

    override val detect = ctx =>
      ctx.foreachLiveReq(() => detectInReqs(ctx))

    private def detectInReqs(ctx: Ctx): Req => Unit = {
      val tagRefs = ctx.project.atomScan.tagRefs
      req => {
        for (a <- tagRefs(req.id).live) {
          val t = ctx.project.config.tags.atag(a.value)
          if (t.live.is(Dead))
            ctx.add(Issue.DeadTag(req.id, a.loc, a.value))
        }
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object EmptyCodeGroup extends Instance {

    override val detect = ctx =>
      ctx.foreachLiveRcg(() => detectInRcgs(ctx))

    private def detectInRcgs(ctx: Ctx): LiveCodeGroup => Unit = {
      val reqCodes = ctx.project.content.reqCodes
      rcg => {
        val code    = reqCodes.reqCodeGroupsById(rcg.id)
        val subtree = reqCodes.trie.getNode(code).get
        val empty   = subtree.valueIterator().forall(isEmpty)
        if (empty)
          ctx.add(Issue.EmptyCodeGroup(rcg.id))
      }
    }

    private val isEmpty: ReqCode.Data => Boolean = {
      case _: ReqCode.ActiveReq   => false
      case _: ReqCode.ActiveGroup
         | _: ReqCode.Inactive    => true
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object ImplicationRequired extends Instance {
    override val detect = ctx => {
      val reqTypeIds = ctx.project.config.reqTypes.idsRequiringImplication
      val imps       = ctx.project.content.implications(Backwards)
      if (reqTypeIds.nonEmpty)
        ctx.foreachLiveReq(() => req => {
          def requiresImp = reqTypeIds.contains(req.reqTypeId)
          def hasNoImps   = imps(req.id).isEmpty
          if (requiresImp && hasNoImps)
            ctx.add(Issue.ImplicationRequired(req.id))
        })
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object IssueTags extends Instance {

    override val detect = ctx => {
      ctx.foreachLiveReq(() => detectInReqs(ctx))
      ctx.foreachLiveRcg(() => detectInRcgs(ctx))
    }

    private def detectInReqs(ctx: Ctx): Req => Unit = {
      val issuesInReqs = ctx.project.atomScan.issuesInReqs
      req => {
        for (a <- issuesInReqs(req.id).live) {
          val t = ctx.project.config.customIssueType(a.value.typ)
          val r = t.live match {
            case Live => Issue.IssueTagInReq(req.id, a.loc, a.value)
            case Dead => Issue.DeadIssueTagInReq(req.id, a.loc, a.value)
          }
          ctx.add(r)
        }
      }
    }

    private def detectInRcgs(ctx: Ctx): LiveCodeGroup => Unit = {
      val issuesInRcgs = ctx.project.atomScan.issuesInRcgs
      rcg => {
        for (a <- issuesInRcgs(rcg.id).live) {
          val t = ctx.project.config.customIssueType(a.typ)
          val r = t.live match {
            case Live => Issue.IssueTagInRcg(rcg.id, a)
            case Dead => Issue.DeadIssueTagInRcg(rcg.id, a)
          }
          ctx.add(r)
        }
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object LooseIssue extends Instance {

    override val detect = ctx =>
      ()
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object UninhabitableTagField extends Instance {

    override val detect = ctx => {
      val cfg = ctx.project.config
      for (f <- ctx.project.config.fields.customTagFields) {
        val isLive        = f.liveExplicitly is Live
        def uninhabitable = !inhabitable(f.tagId, cfg)
        if (isLive && uninhabitable)
          ctx.add(Issue.UninhabitableTagField(f.id))
      }
    }

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
