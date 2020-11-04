package shipreq.webapp.member.issue

import japgolly.microlibs.adt_macros.AdtMacros
import nyaya.util.Multimap
import scala.collection.mutable
import shipreq.base.util._
import shipreq.webapp.member.data._
import shipreq.webapp.member.data.derivation._
import shipreq.webapp.member.text.{Atom, Text}

object IssueDetectors {
  import IssueDetector.Ctx

  sealed trait Instance extends IssueDetector

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object BlankCustomField extends Instance {

    override val detect = ctx => {
      val fields = ctx.project.config.liveCustomFieldsWithMandatory

      // Check imps
      run(ctx, fields.imps) {
        val byField = ctx.project.dataLogic.customFieldImps(HideDead)
        f => {
          val imps = byField(f.id)
          imps.getReqIds(_).isEmpty
        }
      }

      // Check tags
      run(ctx, fields.tags) {
        val tags = ctx.project.virtualTags
        f => tags(_, HideDead).set(f.id).isEmpty
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
          val rulesRT = ctx.project.config.fieldRules(HideDead)
          req => {
            val reqId     = req.id
            val reqTypeId = req.reqTypeId
            val rules     = rulesRT(reqTypeId)
            for ((field, hasIssue) <- as)
              if (rules(field.id).isMandatory && hasIssue(reqId))
                ctx.add(Issue.BlankCustomField(req, field))
          }
        })
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object BlankTitle extends Instance {
    override val detect = ctx =>
      ctx.foreachLiveReq(() => req =>
        if (req.title.isEmpty)
          ctx.add(Issue.BlankTitle(req)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object BlankUseCaseStep extends Instance {
    override val detect = ctx =>
      ctx.foreachLiveUcs(() => f =>
        if (
          f.step.titleExplicitly.isEmpty
          && !f.usesUseCaseTitle
          && f.flow(Forwards, Live).isEmpty
          && f.flow(Backwards, Live).isEmpty
        )
          ctx.add(Issue.BlankUseCaseStep(f)))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object ConflictingTags extends Instance {

    override val detect = ctx =>
      ctx.foreachLiveReq(() => detectInReqs(ctx))

    private def detectInReqs(ctx: Ctx): Req => Unit = {
      val exclusiveGroups = ctx.project.config.tags.exclusiveGroups
      val tags            = ctx.project.virtualTags
      req => {
        val reqId   = req.id
        val results = tags(reqId)
        for (g <- results.conflictingTagGroups) {
          val locs: Set[LocationOf.Tag.InReq] =
            results.conflictingTags.m
              .iterator
              .filter(x => exclusiveGroups(x._1).contains(g))
              .flatMap(_._2)
              .toSet
          ctx.add(Issue.ConflictingTags(req, g, NonEmptySet force locs))
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
            ctx.add(Issue.DeadRefInReq(req, a.loc, ContentRef.fromAtom(a.value)))
    }

    private def detectInRcgs(ctx: Ctx): LiveCodeGroup => Unit = {
      val refsInRcgs = ctx.project.atomScan.contentRefsInRcgs
      rcg =>
        for (a <- refsInRcgs(rcg.id).live)
          if (!isRefLive(a.value, ctx.project))
            ctx.add(Issue.DeadRefInRcg(rcg, ContentRef.fromAtom(a.value)))
    }

    private def isRefLive(a: Atom.AnyContentRef, p: Project): Boolean =
      a match {
        case r: Atom.ContentRef # ReqRef         => p.content.reqs.need(r.id).live(p.config.reqTypes).is(Live)
        case r: Atom.ContentRef # CodeRef        => p.content.reqCodes.needById(r.id).isActive
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
          val t = ctx.project.config.tags.needApplicableTag(a.value)
          if (t.live.is(Dead))
            a.loc match {
              case txtLoc: LocationOf.Text.InReq => ctx.add(Issue.DeadTag(req, txtLoc, t))
              case _                             => ()
            }
        }
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object DerivativeTagRules extends Instance {
    // Note: This only check rule result tags.
    // There's no need to check the keys as they are managed, in that they're automatically hidden from all views until
    // the tags come back in scope.
    override val detect = ctx => {
      val cfg = ctx.project.config
      for (f <- cfg.fields.customTagFields)
        if (f.derivativeTags.enabled.is(Enabled) & f.live(cfg).is(Live)) {
          val scope = cfg.liveTagFieldDistribution.inField(f.id)
          for ((k, v) <- f.derivativeTags.rules) {
            def k1  = cfg.tags.needApplicableTag(k.lo)
            def k2  = cfg.tags.needApplicableTag(k.hi)
            val tag = cfg.tags.needApplicableTag(v)

            if (!scope.contains(v)) {
              val tg = cfg.tags.needTagGroup(f.tagId)
              ctx.add(Issue.DerivativeTagResultUnrelated(f, tg, k1, k2, tag))
            } else if (tag.live is Dead)
              ctx.add(Issue.DerivativeTagResultDead(f, k1, k2, tag))
          }
        }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object DuplicateTitle extends Instance {
    override val detect = ctx =>
      ctx.foreachLiveReq(() => detectInReqs(ctx))

    private def detectInReqs(ctx: Ctx): Req => Unit = {
      val seenPerReqType = mutable.Map.empty[ReqTypeId, mutable.Map[Text.AnyOptional, Req]]

      req => {
        val title = req.title
        if (title.nonEmpty) {
          val seen = seenPerReqType.getOrElseUpdate(req.reqTypeId, mutable.Map.empty)
          val reqSeen = seen.getOrElseUpdate(title, req)
          if (reqSeen ne req) {
            // Found a duplicate
            if (reqSeen ne null) {
              ctx.add(Issue.DuplicateTitle(reqSeen))
              seen.put(title, null)
            }
            ctx.add(Issue.DuplicateTitle(req))
          }
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
          ctx.add(Issue.EmptyCodeGroup(rcg))
      }
    }

    private val isEmpty: ReqCode.Data => Boolean = {
      case _: ReqCode.ActiveReq   => false
      case _: ReqCode.ActiveGroup
         | _: ReqCode.Inactive    => true
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object FieldDefaultTagDead extends Instance {
    override val detect = ctx => {
      val cfg = ctx.project.config
      for (f <- cfg.fields.customTagFields)
        if (f.live(cfg) is Live) {

          var found = Multimap.empty[ApplicableTagId, List, ReqTypeId]
          val fixed = cfg.tagFieldRulesFixedHideDead(f.id)

          fixed.errors.foreach {
            case (reqTypeIdO, ProjectConfig.TagFieldIssue.DefaultTagDead(tag)) =>
              reqTypeIdO match {
                case Some(reqTypeId) =>
                  found = found.add(tag.id, reqTypeId)
                case None =>
                  for (reqTypeId <- cfg.reqTypes.liveIds -- fixed.original.perReqType.keys)
                    found = found.add(tag.id, reqTypeId)
              }

            case _ =>
          }

          for (tagId <- found.keys) {
            val tag = cfg.tags.needApplicableTag(tagId)
            ctx.add(Issue.FieldDefaultTagDead(f, tag))
          }
        }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object FieldDefaultTagNotApplicable extends Instance {
    override val detect = ctx => {
      val cfg = ctx.project.config
      for (f <- cfg.fields.customTagFields)
        if (f.live(cfg) is Live) {

          val fixed = cfg.tagFieldRulesFixedHideDead(f.id)

          fixed.errors.valuesIterator.foreach {
            case ProjectConfig.TagFieldIssue.DefaultTagNotApplicable(tag, reqType) =>
              ctx.add(Issue.FieldDefaultTagNotApplicable(f, tag, reqType))

            case _ =>
          }
        }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object FieldDefaultTagUnrelated extends Instance {
    override val detect = ctx => {

      val cfg = ctx.project.config
      for (f <- cfg.fields.customTagFields)
        if (f.live(cfg) is Live) {

          val unrelatedTags: Set[ApplicableTagId] =
            cfg
              .tagFieldRulesFixedHideDead(f.id)
              .errors
              .valuesIterator
              .collect { case ProjectConfig.TagFieldIssue.DefaultTagUnrelated(tag) => tag.id }
              .toSet

          for (tagId <- unrelatedTags) {
            val tag = cfg.tags.needApplicableTag(tagId)
            ctx.add(Issue.FieldDefaultTagUnrelated(f, tag))
          }
        }
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
            ctx.add(Issue.ImplicationRequired(req))
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
          val t = ctx.project.config.customIssueTypes.need(a.value.typ)
          val r = t.live match {
            case Live => Issue.IssueTagInReq(req, a.loc, a.value)
            case Dead => Issue.DeadIssueTagInReq(req, a.loc, a.value)
          }
          ctx.add(r)
        }
      }
    }

    private def detectInRcgs(ctx: Ctx): LiveCodeGroup => Unit = {
      val issuesInRcgs = ctx.project.atomScan.issuesInRcgs
      rcg => {
        for (a <- issuesInRcgs(rcg.id).live) {
          val t = ctx.project.config.customIssueTypes.need(a.typ)
          val r = t.live match {
            case Live => Issue.IssueTagInRcg(rcg, a)
            case Dead => Issue.DeadIssueTagInRcg(rcg, a)
          }
          ctx.add(r)
        }
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object NonApplicableTag extends Instance {

    override val detect = ctx =>
      ctx.foreachLiveReq(() => detectInReqs(ctx))

    private def detectInReqs(ctx: Ctx): Req => Unit = {
      val tags = ctx.project.virtualTags
      req => {
        for {
          (tagId, locs) <- tags(req.id).naTagsInLiveText.m
          tag            = ctx.project.config.tags.needApplicableTag(tagId)
          loc           <- locs
        }
          ctx.add(Issue.NonApplicableTag(req, loc, tag))
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object NonApplicableField extends Instance {
    override val detect = ctx => {
      val cfg = ctx.project.config
      for (f <- cfg.fields.customFields.values) {
        val isLive      = f.liveExplicitly is Live
        def allDeadOrNA = f.fieldReqTypeRules.liveResolutionIterator(cfg.reqTypes).forall(_.isNA)
        if (isLive && allDeadOrNA)
          ctx.add(Issue.NonApplicableField(f))
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  case object UninhabitableTagField extends Instance {

    override val detect = ctx => {
      val cfg = ctx.project.config
      for (f <- cfg.fields.customTagFields) {
        val isLive        = f.liveExplicitly is Live
        def uninhabitable = !inhabitable(f.tagId, cfg)
        if (isLive && uninhabitable)
          ctx.add(Issue.UninhabitableTagField(f))
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
