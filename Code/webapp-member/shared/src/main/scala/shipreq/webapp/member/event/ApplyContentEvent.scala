package shipreq.webapp.member.event

import japgolly.microlibs.nonempty.NonEmpty
import monocle.Lens
import scalaz.std.option.optionInstance
import shipreq.base.util.MTrie.Ops
import shipreq.base.util._
import shipreq.webapp.member.data.DataImplicits._
import shipreq.webapp.member.data._
import shipreq.webapp.member.event.ApplyEventLib._
import shipreq.webapp.member.event.Event._
import shipreq.webapp.member.text.Text

trait ApplyContentEvent {
  this: ApplyEvent =>

  object ContentCommon {
    val grIMap = IMapStore(Project.genericReqs)
    val ucIMap = IMapStore(Project.useCaseIMap)

    private val grLiveExplicitly = LiveAccessor(GenericReq.liveExplicitly)(_.id.toString)
    private val ucLiveExplicitly = LiveAccessor(UseCase.liveExplicitly)(_.id.toString)

    val updateReqIdCeiling = updateIdCeilingFn(IdCeilings.req)

    def ensureLiveReqId(reqId: ReqId): Eval[Unit] =
      whenUntrusted(
        needReq(reqId).flatMap(ensureLiveReq))

    def ensureLiveReq(req: Req): Eval[Unit] =
      whenUntrusted(
        Eval.tests(p => req.live(p.config.reqTypes) is Live, s"${show(req)} is dead."))

    def ensureLiveTextFieldId(id: CustomField.Text.Id): Eval[Unit] =
      whenUntrusted(
        Eval.eithers(_.config.fields.customAttempt(id)).flatMap(ensureLiveTextField))

    def ensureLiveTextField(cf: CustomField.Text): Eval[Unit] =
      Eval.getFlatMap(p => ensureLive(cf live p.config)(show(cf)))

    def needReq[T <: ReqTypeId](reqId: ReqIdT[T]): Eval[Req] =
      reqId match {
        case id: GenericReqId => grIMap.need(id).widen[Req]
        case id: UseCaseId    => ucIMap.need(id).widen[Req]
      }

    private def deleteReq(id: ReqId): Eval[Unit] =
      id match {
        case gr: GenericReqId => grIMap.update(gr, grLiveExplicitly.makeDead)
        case uc: UseCaseId    => ucIMap.update(uc, ucLiveExplicitly.makeDead)
      }

    private def restoreReq(id: ReqId): Eval[Unit] =
      id match {
        case gr: GenericReqId => grIMap.update(gr, grLiveExplicitly.makeLive)
        case uc: UseCaseId    => ucIMap.update(uc, ucLiveExplicitly.makeLive)
      }

    /** Ignores reqcodes */
    def restore(reqIds: Iterable[ReqId]): Eval[Unit] =
      Eval.foldMapRun(reqIds)(restoreReq)

    def applyDelete(e: ReqsDelete): Eval[Unit] = {
      val reqIds = e.reqs.whole
      for {
        _  <- Eval.foldMapRun(reqIds)(deleteReq)
        t1 <- ReqCodeLogic.getTrie
        t2 <- ReqCodeLogic.inactivateBelongingToReqsT(t1, reqIds)
        t3 <- ReqCodeLogic.inactivateGroupsByIdT(t2, e.codeGroups, remember = true)
        _  <- Project.reqCodeTrie set t3
        _  <- Project.deletionReasons modify (addDeletionReason(_, e.reason, e.reqs))
      } yield ()
    }

    private def addDeletionReason(dr    : DeletionReasons,
                                  reason: Text.DeletionReason.OptionalText,
                                  reqIds: NonEmptySet[ReqId]): DeletionReasons = {

      def noReason: DeletionReasons = {
        var r = dr.reqApplication
        for (reqId <- reqIds) {
          val prev = r(reqId)
          if (prev.nonEmpty && prev.last.isDefined) // No last reason means nothing to change
            r = r.add(reqId, None)
        }
        DeletionReasons(dr.reasons, r)
      }

      def hasReason(r: Text.DeletionReason.NonEmptyText): DeletionReasons = {
        val id = Some(DeletionReasonId(dr.reasons.length))
        DeletionReasons(
          dr.reasons :+ r,
          reqIds.foldLeft(dr.reqApplication)(_.add(_, id)))
      }

      NonEmptyArraySeq.maybe(reason, noReason)(hasReason)
    }

    def validateTags(tagIds: => Iterable[ApplicableTagId]): Eval[Unit] =
      whenUntrusted(
        Eval.failOptions(p =>
        tagIds.iterator.map(p.config.tags.validateApplicableTag).find(_.isDefined) match {
          case Some(None) | None => None
          case Some(Some(err))   => Some(err)
        }
      ))

    def applyReqTagsPatch(e: ReqTagsPatch): Eval[Unit] =
      ensureLiveReqId(e.id) >>
      validateTags(e.patch.value.allValues) >>
      Project.reqTags.modify(_.mod(e.id, e.patch.value.apply))

    def applyReqImplicationsPatch(e: ReqImplicationsPatch): Eval[Unit] =
      ensureLiveReqId(e.id) >>
        Project.implicationsSrcToTgt.modify(is =>
          e.dir match {
            case Forwards  => e.patch.value.applyToMultimapValues(is)(e.id)
            case Backwards => e.patch.value.applyToMultimapKeys  (is)(e.id)
          }
        )

    def applyReqFieldCustomTextSet(e: ReqFieldCustomTextSet): Eval[Unit] =
      ensureLiveReqId(e.id) >> setCustomTextValue(e.id, e.fid, e.value)

    def applyRestoreContent(e: ContentRestore): Eval[Unit] =
      for {
        _  <- ContentCommon.restore(e.reqs)
        t1 <- ReqCodeLogic.getTrie
        t2 <- ReqCodeLogic.restoreBelongingToReqsT(t1, e.reqs)
        t3 <- ReqCodeLogic.restoreGroupsByIdT(t2, e.codeGroups)
        _  <- Project.reqCodeTrie set t3
      } yield ()

    def setCustomTextValue(id   : ReqId,
                           fid  : CustomField.Text.Id,
                           value: Text.CustomTextField.OptionalText): Eval[Unit] =
      ensureLiveTextFieldId(fid) >> (Project.reqText ^|-> ReqData.Text.at(fid, id)).set(value)

    def setCustomTextValueMap(id: ReqId,
                              values: NonEmpty[Map[CustomField.Text.Id, Text.CustomTextField.NonEmptyText]]): Eval[Unit] =
      values.iterator
        .map { case (fid, value) => setCustomTextValue(id, fid, value.whole) }
        .reduce(_ >> _)

    def setReqCodes(id: ReqId, v: NonEmptySet[ApReqCodeId.AndValue]): Eval[Unit] =
      ReqCodeLogic.addAllToReq(v.whole, id, addToActive = true)

    def setReqTags(id: ReqId, v: NonEmptySet[ApplicableTagId]): Eval[Unit] =
      Project.reqTags.modify(_.addvs(id, v.whole))

    def setReqImpSrcs(id: ReqId, v: NonEmptySet[ReqId]): Eval[Unit] =
      Project.implicationsSrcToTgt.modify(_.addks(v.whole, id))

    def setReqImpTgts(id: ReqId, v: NonEmptySet[ReqId]): Eval[Unit] =
      Project.implicationsSrcToTgt.modify(_.addvs(id, v.whole))
  }

  // ===================================================================================================================

  object GenericReqEvents {
    import ContentCommon.{ucIMap => _, _}

    def ensureLiveCustomReqType(rt: CustomReqType): Eval[Unit] =
      ensureLive(rt.live)(show(rt))

    def ensureLiveCustomReqTypeId(id: CustomReqTypeId): Eval[Unit] =
      whenUntrusted(needCustomReqType(id).flatMap(ensureLiveCustomReqType))

    def needCustomReqType(id: CustomReqTypeId): Eval[CustomReqType] =
      CustomReqTypeEvents.imap need id

    def needLiveCustomReqType(id: CustomReqTypeId): Eval[CustomReqType] =
      needCustomReqType(id).flatTap(rt => ensureLive(rt.live)(rt.mnemonic.value))

    val applyGenericReqCreate: GenericReqCreate => Eval[Unit] = {
      val ^ = GenericReqGD

      def foreachValue(id: GenericReqId, vs: ^.Values): Eval[Unit] =
        Eval.foldMapRun(vs.values) {
          case ^.ValueForCodes     (v) => setReqCodes          (id, v)
          case ^.ValueForCustomText(v) => setCustomTextValueMap(id, v)
          case ^.ValueForImpSrcs   (v) => setReqImpSrcs        (id, v)
          case ^.ValueForImpTgts   (v) => setReqImpTgts        (id, v)
          case ^.ValueForTags      (v) => setReqTags           (id, v)
          case ^.ValueForTitle     (_) => Eval.unit // Handled below
        }

      @inline def emptyTitle: Text.GenericReqTitle.OptionalText =
        Text.empty

      e => for {
        rt      <- needLiveCustomReqType(e.rt)
        reqData <- Eval.gets(_.content.reqs)
        id      = e.id
        title   = ^.Title.get(e.vs).fold(emptyTitle)(_.value.whole)
        pp      = reqData.pubids.allocGR(rt.id)(id)
        req     = GenericReq(id, pp._2, title, Live)
        _       <- grIMap.create(req)
        _       <- Project.pubidRegister set pp._1
        _       <- foreachValue(id, e.vs)
        _       <- updateReqIdCeiling(id)
      } yield ()
    }

    def applyGenericReqTypeSet(e: GenericReqTypeSet): Eval[Unit] =
      for {
        r <- grIMap.need(e.id)
        _ <- ensureLiveReq(r)
        _ <- ensureLiveCustomReqTypeId(e.value)
        _ <- Project.reqs.modify { reqs =>
               val pp = reqs.pubids.allocGR(e.value)(e.id)
               val r2 = r.copy(pubid = pp._2)
               val grs = GenericReqs(reqs.genericReqs.imap + r2)
               Requirements(grs, reqs.useCases, pp._1)
             }
      } yield ()

    def applyGenericReqTitleSet(e: GenericReqTitleSet): Eval[Unit] =
      ensureLiveReqId(e.id) >>
        grIMap.updateF(e.id, _.copy(title = e.value))
  }

  // ===================================================================================================================

  object UseCaseEvents {
    import ContentCommon.{grIMap => _, _}
    import StaticField.{UseCaseStepTree => StepField, NormalAltStepTree => NCAC}
    import VectorTree.Location

    val StepFlowUni: Lens[Project, UseCases.StepFlow.UniDir] =
      Project.useCases ^|-> UseCases.stepFlow ^<-> UseCases.StepFlow.biToUni

    @inline def ensureStepExists(id: UseCaseStepId): Eval[Unit] =
      ensureStepExistence(id, true)

    @inline def ensureStepDoesntExist(id: UseCaseStepId): Eval[Unit] =
      ensureStepExistence(id, false)

    def ensureStepExistence(id: UseCaseStepId, expectToExist: Boolean): Eval[Unit] =
      whenUntrusted(Eval.tests(
        _.content.reqs.useCases.stepIndex.contains(id) ==* expectToExist,
        show(id) + (if (expectToExist) " not found." else " already exists.")))

    def ensureStepsExist(_ids: => Iterable[UseCaseStepId]): Eval[Unit] =
      whenUntrusted(Eval.failOptions { p =>
        val si = p.content.reqs.useCases.stepIndex
        val ids = _ids
        if (ids forall si.contains)
          None
        else {
          val bad = ids.toSet -- si.keySet
          Some(bad.toList.map(_.value).sorted.mkString("Step(s) not found: {", ",","}."))
        }
      })

    def applyTitleSet(e: UseCaseTitleSet): Eval[Unit] =
      ensureLiveReqId(e.id) >>
        ucIMap.updateF(e.id, _.copy(title = e.value))

    def postAddStep(ucId: UseCaseId, stepId: UseCaseStepId, field: StepField): Eval[Unit] =
      postAddStep(ucId, stepId, field,
        (r, ucs) => r.copy(useCases = ucs),
        (ic, i) => ic.copy(useCaseStep = i))

    def postAddStep(ucId: UseCaseId, stepId: UseCaseStepId, field: StepField,
                    updReqs: (Requirements, UseCases) => Requirements,
                    updStepIdCeil: (IdCeilings, Int) => IdCeilings): Eval[Unit] =
      Eval.mod { p =>
        // Update step index
        val ptr = UseCases.StepTreeKey(ucId, field)
        val si2 = p.content.reqs.useCases.stepIndex.updated(stepId, ptr)
        val ucs2 = p.content.reqs.useCases.copy(stepIndex = si2)

        // Update reqs
        val reqs2 = updReqs(p.content.reqs, ucs2)

        // Update ID ceilings
        val ic2 = updStepIdCeil(p.idCeilings, p.idCeilings.useCaseStep max stepId)

        p.copy(content = p.content.copy(reqs = reqs2), idCeilings = ic2)
      }

    val applyCreate: UseCaseCreate => Eval[Unit] = {
      val ^ = UseCaseGD

      def foreachValue(id: UseCaseId, vs: ^.Values): Eval[Unit] =
        Eval.foldMapRun(vs.values) {
          case ^.ValueForCodes     (v) => setReqCodes          (id, v)
          case ^.ValueForCustomText(v) => setCustomTextValueMap(id, v)
          case ^.ValueForImpSrcs   (v) => setReqImpSrcs        (id, v)
          case ^.ValueForImpTgts   (v) => setReqImpTgts        (id, v)
          case ^.ValueForTags      (v) => setReqTags           (id, v)
          case ^.ValueForTitle     (_) => Eval.unit // Handled below
        }

      def postAdd(pr: PubidRegister, ucId: UseCaseId, stepId: UseCaseStepId): Eval[Unit] =
        postAddStep(ucId, stepId, NCAC,
          (r, ucs) => r.copy(useCases = ucs, pubids = pr),
          (ic, i) => ic.copy(useCaseStep = i, req = ic.req max ucId))

      e => for {
        _      <- ensureStepDoesntExist(e.stepId)
        id      = e.id
        title   = ^.Title.get(e.vs).fold(Text.UseCaseTitle.empty)(_.value.whole)
        pp     <- Eval.gets(_.content.reqs.pubids allocUC id)
        uc      = UseCase.empty(id, pp._2.pos, title, e.stepId)
        _      <- ucIMap.create(uc)
        _      <- postAdd(pp._1, id, e.stepId)
        _      <- foreachValue(id, e.vs)
      } yield ()
    }

    def applyStepCreate(e: UseCaseStepCreate): Eval[Unit] = {
      val step = UseCaseStep(e.id, Text.UseCaseStep.empty, Live)
      val tree = e.field.useCaseStepTree

      def insert(loc: Location, uc: UseCase): Eval[Unit] =
        tree.modifyF(_.insertAfter(loc, step))(uc) match {
          case Some(uc2) => ucIMap addOrUpdate uc2
          case None =>
            val ploc = VectorTree.PartialLocation(loc, Valid)
            val locStr = e.field.stepLabel(uc.pos, ploc, UseCaseStepLabelFmt.`N.m`)
            Eval.fail(s"${show(step.id)} cannot be added to ${show(uc.id)} at location: $locStr")
        }

      def append(uc: UseCase): Eval[Unit] =
        ucIMap addOrUpdate tree.modify(_ append step)(uc)

      for {
        _  <- ensureStepDoesntExist(step.id)
        uc <- ucIMap.need(e.ucId)
        _  <- ensureLiveReq(uc)
        _  <- e.at.location.fold(append(uc))(insert(_, uc))
        _  <- postAddStep(e.ucId, e.id, e.field)
      } yield ()
    }

    def needStepIndex(id: UseCaseStepId): Eval[UseCases.StepTreeKey] =
      Eval.getFlatMap(p => Eval.some(p.content.reqs.useCases.stepIndex.get(id), s"${show(id)} not found."))

    def needStepFocus(id: UseCaseStepId): Eval[UseCaseStep.Focus] =
      needStepIndex(id) >> Eval.gets(_.content.reqs.useCases.focusStep(id))

    private def findStepModTree(id: UseCaseStepId)(mod: (UseCaseSteps, StepField, Location, UseCaseStep) => Eval[UseCaseSteps.Tree]): Eval[Unit] =
      needStepIndex(id).flatMap { idx =>
        val ucId = idx.useCaseId
        val f    = idx.field
        // It's fast to use findLoc below to find the step location rather than using some lazy index because
        // 1) The index would need to be recalculated every time a step in the tree is changed
        //    (meaning the index is only used once before being discarded).
        // 2) findLoc will stop when it finds the step. An index scans the whole tree.
        ucIMap.update(ucId, uc =>
          ensureLiveReq(uc) >>
            Eval.some(f.useCaseSteps.get(uc).tree.findLocAndValue(_.id ==* id), s"${show(id)} not found.")
              .flatMap(ls => f.useCaseSteps.modifyF[Eval](t => mod(t, f, ls._1, ls._2).map(UseCaseSteps(_)))(uc)))
      }

    private def mapStep(id: UseCaseStepId)(mod: UseCaseStep => UseCaseStep): Eval[Unit] =
      findStepModTree(id)((steps, _, l, s) =>
        setStep(steps.tree, l)(mod(s)))

    private def setStep(t: UseCaseSteps.Tree, loc: Location)(s: UseCaseStep): Eval[UseCaseSteps.Tree] =
      Eval.some(t.modifyValueAt(loc)(_ => s), badStepIndex(s.id, loc))

    def applyStepShiftLeft(e: UseCaseStepShiftLeft): Eval[Unit] =
      findStepModTree(e.id)((s, _, l, _) =>
        Eval.some(s.tree shiftLeft l, s"${show(e.id)} cannot be shifted left."))

    def applyStepShiftRight(e: UseCaseStepShiftRight): Eval[Unit] =
      findStepModTree(e.id)((s, _, l, _) =>
        Eval.some(s.tree.shiftRightV(l, s.locValidity), s"${show(e.id)} cannot be shifted right."))

    private def badStepIndex(id: UseCaseStepId, loc: Location) =
      s"${show(id)} expected at ${showLoc(loc)}."

    val applyStepUpdate: UseCaseStepUpdate => Eval[Unit] = {
      val ^ = UseCaseStepGD

      val noFlow = SetDiff.empty[UseCaseStepId]

      def getFlow(values: UseCaseStepGD.Values, a: UseCaseStepGD.Attr {type Data = SetDiff.NE[UseCaseStepId]}): SetDiff[UseCaseStepId] =
        a.get(values).fold(noFlow)(_.value)

      def updateFlow(id: UseCaseStepId, flow_<- : SetDiff[UseCaseStepId], flow_-> : SetDiff[UseCaseStepId]): Eval[Unit] =
        if (flow_<-.isEmpty && flow_->.isEmpty)
          Eval.unit
        else
          ensureStepsExist(flow_->.allValues ++ flow_<-.allValues) >>
          StepFlowUni.modify { f0 =>
            var f = f0
            f = flow_->.applyToMultimapValues(f)(id)
            f = flow_<-.applyToMultimapKeys(f)(id)
            f
          }

      def updateTitle(id: UseCaseStepId, title: Text.UseCaseStep.OptionalText): Eval[Unit] =
        mapStep(id)(_.copy(titleExplicitly = title))

      e => {
        val gd       = e.vs.value
        def updFlow  = updateFlow(e.id, getFlow(gd, ^.FlowIn), getFlow(gd, ^.FlowOut))
        def updTitle = ^.Title.get(gd).fold(Eval.unit)(v => updateTitle(e.id, v.value))

        ensureStepExists(e.id) >> updFlow >> updTitle
      }
    }

    def applyStepDelete(e: UseCaseStepDelete): Eval[Unit] =
      for {
        f   <- needStepFocus(e.id)
        _   <- whenUntrusted(Eval.test(f.field.canDelete(f.loc) is Allow, s"Deletion of step ${show(e.id)} forbidden."))
        del <- Eval.gets(_.deletionMethodForUseCaseStep(e.id))
        _   <- del match {
                 case DeletionMethod.Soft => setUseCaseStepLive(e.id, Dead)
                 case DeletionMethod.Hard => hardDeleteStep(f)
               }
      } yield ()

    private def hardDeleteStep(f: UseCaseStep.Focus): Eval[Unit] =
      for {
        _ <- findStepModTree(f.id)((steps, _, loc, _) => Eval.pure(steps.tree.remove(loc).get))
        _ <- postHardDelete(f.subtree)
      } yield ()

    private def postHardDelete(deleted: VectorTree.Node[UseCaseStep]): Project => Project =
      Project.useCases.modify { ucs =>
        def ids = deleted.valueIterator.map(_.id)
        val si2 = ucs.stepIndex -- ids
        ucs.copy(stepIndex = si2)
      }

    def applyStepRestore(e: UseCaseStepRestore): Eval[Unit] =
      setUseCaseStepLive(e.id, Live)

    private def setUseCaseStepLive(id: UseCaseStepId, life: Live): Eval[Unit] =
      findStepModTree(id)((steps, _, loc, step) =>
        ensureLiveIsNot(step.liveIgnoringUC(steps))(life, show(id)) >>
          setStep(steps.tree, loc)(step.copy(liveExplicitly = life)))
  }

  // ===================================================================================================================
  object CodeGroupEvents {
    import ApplyReqCodeLogic._
    import ReqCodeLogic._

    val ^ = CodeGroupGD
    val GD = GenericDataApp[ReqCodes](^)

    def applyCreate(e: CodeGroupCreate): Eval[Unit] = {
      implicit val vs = e.vs
      for {
        c <- GD.need(^.Code)
        t = GD.want(^.Title)(Text.empty)
        g = LiveCodeGroup(e.id, t)
        _ <- addOne(AddGroup(c, Unvalidated, g))
      } yield ()
    }

    private def updateGroupCode(id: ReqCodeId, newCode: ReqCode.Value): Eval[Unit] =
      for {
        t  <- getTrie
        v  <- needCode(id)
        d  <- needData(t, v)
        ag <- needActiveGroup(d, v)
        t2 = inactivateGroup(t, v, ag, false)
        t3 <- addOneT(t2, AddGroup(newCode, Unvalidated, ag.group))
        _  <- Project.reqCodeTrie set t3
      } yield ()

    private def modifyGroup(id: ReqCodeId, f: LiveCodeGroup => LiveCodeGroup): Eval[Unit] =
      for {
        t  <- getTrie
        v  <- needCode(id)
        d  <- needData(t, v)
        ag <- needActiveGroup(d, v)
        d2 = ReqCode.ActiveGroup.group.modify(f)(ag)
        t2 = t.put(v, d2)
        _  <- Project.reqCodeTrie set t2
      } yield ()

    def applyUpdate(e: CodeGroupUpdate): Eval[Unit] =
      Eval.foldMapRun(e.vs.values) {
        case ^.ValueForTitle(t) => modifyGroup(e.id, _.copy(title = t))
        case ^.ValueForCode (v) => updateGroupCode(e.id, v)
      }

    def applyDelete(e: CodeGroupsDelete): Eval[Unit] =
      getTrie.flatMap(inactivateGroupsByIdT(_, e.ids.whole, true)).flatMap(Project.reqCodeTrie.set)
  }
}
