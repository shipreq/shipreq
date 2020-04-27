package shipreq.webapp.base.event

import japgolly.microlibs.nonempty._
import monocle.Lens
import nyaya.util.Multimap
import scala.annotation.tailrec
import scalaz.std.option.optionInstance
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{DataValidators => V, _}
import shipreq.webapp.base.text.{Grammar, Text}
import shipreq.webapp.base.validation.Implicits._
import ApplyEventLib._
import DataImplicits._
import Event._
import MTrie.Ops

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
      Eval.foldMapRun(reqIds)(restoreReq) // TODO Use one Project get/put

    def applyDelete(e: ReqsDelete): Eval[Unit] = {
      val reqIds = e.reqs.whole
      for {
        _  <- Eval.foldMapRun(reqIds)(deleteReq) // TODO Use one Project get/put
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
        pp      = reqData.pubids.allocC(rt.id)(id)
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
               val pp = reqs.pubids.allocC(e.value)(e.id)
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

  /**
   * Why the hell is all this req-code changing logic so complicated?
   *
   * Because req codes aren't true, unique identifiers to requirements. They act as such when they're active, but
   * they often change, get reassigned, and become inactive.
   *
   * And then there are code references that can exist in text.
   * Basically, a reference to req via a code always maintains its link to said req, and tries to keep and follow the
   * code around as it changes, without disrupting other workflows.
   *
   * Thus, the complicated logic is in place to achieve the following properties:
   *
   * - A ref to req via code *never* loses the association to the original req.
   * - Refer via code to a req/group, rename code, refs appear to be updated.
   * - Refer via code to a req/group, del code, restore code, ref shows original code.
   * - Refer via code to a req, aggregate req codes, refs appear to be updated.
   * - Refer via code to a req, del code, ref displayed using other code or pubid.
   * - Refer via code to a group, del code, ref displayed as an error.
   * - Delete a req, another req can use its req code.
   * - Delete a req, restore it, it retains its req codes (unless they've been usurped meaning other they're active
   *   and assigned to other targets).
   */
  object ReqCodeLogic {
    import ReqCode._

    val updateIdCeiling = updateIdCeilingFn(IdCeilings.reqCode)

    val validateCode = validateA(V.reqCode.valueAndNodes named SpecialBuiltInField.Code.name)

    val getTrie = Eval.gets(Project.reqCodeTrie.get)

    def ensureInactive(d: Data, v: Value): Eval[Unit] =
      whenUntrusted(d match {
        case _: Inactive    => Eval.unit
        case _: ActiveReq
           | _: ActiveGroup => Eval.fail(s"${show(v)} shouldn't be in use.")
      })

    def ensureActiveReqIs(reqId: ReqId): ActiveReq => Eval[Unit] =
      whenUntrusted(a => Eval.test(a.reqId ==* reqId, s"Expected ReqCode target to be $reqId, found: ${a.reqId}."))

    def ensureRefToReqExists(v: Value, d: Data, rc: ApReqCodeId)(reqId: ReqId): Eval[Unit] =
      whenUntrusted(
        Eval.test(d.reqInactive(reqId) contains rc, s"Ref to ${show(reqId)} not found in ${show(v)}."))

    def needData(t: Trie, v: Value): Eval[Data] =
      t.valueAtPath[Eval[Data]](v, Eval.fail(s"${show(v)} not found."))(Eval.pure)

    def needActiveReq(d: Data, v: Value): Eval[ActiveReq] =
      narrowCC[Data, ActiveReq](d, s"${show(v)} is not an ActiveReq.")

    def needActiveGroup(d: Data, v: Value): Eval[ActiveGroup] =
      narrowCC[Data, ActiveGroup](d, s"${show(v)} is not an ActiveGroup.")

    def needDeadGroup(d: Data, v: Value): Eval[DeadCodeGroup] =
      Eval.some(d.deadGroup, s"Expected to find dead group at ${show(v)}.")

    def needCode(id: ReqCodeId): Eval[Value] =
      Eval.getFlatMap(p => Eval.some(p.content.reqCodes.getReqCode(id), s"${show(id)} not found."))

    def needApCodes[A](ids: Iterable[ApReqCodeId], f: (ApReqCodeId, Value) => A): Eval[Vector[A]] =
      needCodes(ids)(_.content.reqCodes.apReqCodesById.get, f)

    def needCodeGroups[A](ids: Iterable[ReqCodeGroupId], f: (ReqCodeGroupId, Value) => A): Eval[Vector[A]] =
      needCodes(ids)(_.content.reqCodes.reqCodeGroupsById.get, f)

    def needCodes[I <: ReqCodeId, A](ids: Iterable[I])
                                    (getFn: Project => I => Option[Value], f: (I, Value) => A): Eval[Vector[A]] = {
      Eval.getFlatMap { p =>
        val get = getFn(p)
        val found = Vector.newBuilder[A]
        var missing = Vector.empty[I]
        for (id <- ids)
          get(id) match {
            case Some(v) => found += f(id, v)
            case None    => missing :+= id
          }
        if (missing.nonEmpty)
          Eval.fail(s"Codes not found: ${missing.map(_.value).sorted.map("#" + _).mkString(", ")}")
        else
          Eval.pure(found.result())
      }
    }

    private def awakenGroup(g: DeadCodeGroup) = LiveCodeGroup(g.id, g.title)
    private def killGroup  (g: LiveCodeGroup) = DeadCodeGroup(g.id, g.title)

    sealed trait Adder[A] {
      def reqCodeId(a: A): ReqCodeId
      def apply(t: Trie, a: A): Eval[Trie]
    }

    /** Command to add a ReqCode to a requirement. */
    case class AddReq(code: Value, codeValidated: Validated, id: ApReqCodeId, reqId: ReqId, addToActive: Boolean)

    implicit object ReqAdder extends Adder[AddReq] {
      override def reqCodeId(a: AddReq) = a.id

      override def apply(t: Trie, cmd: AddReq): Eval[Trie] =
        cmd.codeValidated.mapValidated(cmd.code)(validateCode) { v =>
          type MakeNewData = Eval[ActiveReq]
          import cmd.{addToActive, id, reqId}

          def createNode: MakeNewData =
            if (addToActive)
              Eval.pure(ActiveReq(id, reqId, None, emptyReqInactive))
            else
              Eval.fail(s"${show(v)} not found; can't add inactive ${show(id)} .")

          def modifyNode(d: Data): MakeNewData =
            if (addToActive)
              ensureInactive(d, v)
                .andReturn(ActiveReq(id, reqId, d.deadGroup, d.reqInactive.del(reqId, id)))
            else
              needActiveReq(d, v)
                .map(ar => ar.copy(reqInactive = ar.reqInactive.add(reqId, id)))

          t.valueAtPath(v, createNode)(modifyNode).map(t.put(v, _))
        }
    }

    /** Command to add an active CodeGroup. */
    case class AddGroup(code: Value, codeValidated: Validated, g: LiveCodeGroup)

    implicit object GroupAdder extends Adder[AddGroup] {
      override def reqCodeId(a: AddGroup) = a.g.id

      override def apply(t: Trie, cmd: AddGroup): Eval[Trie] =
        cmd.codeValidated.mapValidated(cmd.code)(validateCode) { v =>
          type MakeNewData = Eval[ActiveGroup]
          import cmd.g

          def createNode: MakeNewData =
            Eval.pure(ActiveGroup(g, emptyReqInactive))

          def modifyNode(d: Data): MakeNewData =
            ensureInactive(d, v).andReturn(ActiveGroup(g, d.reqInactive))

          t.valueAtPath(v, createNode)(modifyNode).map(t.put(v, _))
        }
    }

    def addOne[A](a: A)(implicit adder: Adder[A]): Eval[Unit] =
      getTrie.flatMap(addOneT(_, a)).flatMap(Project.reqCodeTrie.set)

    def addAll[A](as: Iterable[A])(implicit adder: Adder[A]): Eval[Unit] =
      getTrie.flatMap(addAllT(_, as)).flatMap(Project.reqCodeTrie.set)

    def addOneT[A](t: Trie, a: A)(implicit adder: Adder[A]): Eval[Trie] =
      updateIdCeiling(adder.reqCodeId(a).value) >>
        adder(t, a)

    def addAllT[A](t: Trie, as: Iterable[A])(implicit adder: Adder[A]): Eval[Trie] =
      updateIdCeiling(IdCeilings.maxOfF(as)(adder.reqCodeId(_).value)) >>
        foldMapBind(t, as)(a => adder(_, a))

    def addAllToReq(vs: Iterable[ApReqCodeId.AndValue], reqId: ReqId, addToActive: Boolean): Eval[Unit] =
      addAll(vs map (iv =>
        AddReq(iv.value, Unvalidated, iv.id, reqId, addToActive)))

    private def putInactive(trie: Trie, code: Value, data: Inactive): Trie =
      if (data.nonEmpty)
        trie.put(code, data)
      else
        trie.remove(code)

    /**
     * @param remember Determine whether a reference should be kept of the current id and target.
     *                 If `false`, the data is gone completely.
     */
    def inactivateReq(trie: Trie, code: Value, a: ActiveReq, remember: Boolean): Trie = {
      val ri: ReqInactive =
        if (remember)
          a.reqInactive.add(a.reqId, a.id)
        else
          a.reqInactive
      val d2 = Inactive(a.deadGroup, ri)
      putInactive(trie, code, d2)
    }

    def inactivateReqsByCodeT(t: Trie, codes: Iterable[Value], remember: ReqCodeId => Boolean, validateTarget: ActiveReq => Eval[Unit]): Eval[Trie] =
      foldMapBind(t, codes)(v => t =>
        for {
          d <- needData(t, v)
          a <- needActiveReq(d, v)
          _ <- validateTarget(a)
        } yield inactivateReq(t, v, a, remember(a.id))
      )

    def inactivateReqsByIdT(t: Trie, ids: Iterable[ApReqCodeId], remember: ReqCodeId => Boolean, validateTarget: ActiveReq => Eval[Unit]): Eval[Trie] =
      needApCodes(ids, (_, v) => v).flatMap(vs =>
        inactivateReqsByCodeT(t, vs, remember, validateTarget))

    def inactivateBelongingToReqs(reqIds: Set[ReqId]): Eval[Unit] =
      getTrie.flatMap(inactivateBelongingToReqsT(_, reqIds)).flatMap(Project.reqCodeTrie.set)

    def inactivateBelongingToReqsT(trie: Trie, reqIds: Set[ReqId]): Eval[Trie] =
      Eval.gets(_.content.reqCodes.activeReqCodesByReqId).flatMap(m =>
        foldMapBind(trie, reqIds)(reqId => inactivateReqsByCodeT(_, m(reqId), _ => true, ensureActiveReqIs(reqId))))

    /**
     * @param remember Determine whether the group should be moved into the ReqCode's dead group slot.
     *                 A reason for `false` here is when the group is being moved and will be active elsewhere.
     */
    def inactivateGroup(trie: Trie, code: Value, a: ActiveGroup, remember: Boolean): Trie = {
      val dg: DeadGroup =
        if (remember) // Adding "… && a.group.nonEmpty" means "remember" must also check if refs this code exist
          Some(killGroup(a.group))
        else
          None
      val d2 = Inactive(dg, a.reqInactive)
      putInactive(trie, code, d2)
    }

    def inactivateGroupsByIdT(t: Trie, ids: Iterable[ReqCodeGroupId], remember: Boolean): Eval[Trie] =
      needCodeGroups(ids, (_, v) => v).flatMap(vs =>
        inactivateGroupsByCodeT(t, vs, remember))

    def inactivateGroupsByCodeT(t: Trie, codes: Iterable[Value], remember: Boolean): Eval[Trie] =
      foldMapBind(t, codes)(code => t =>
        for {
          d <- needData(t, code)
          a <- needActiveGroup(d, code)
        } yield inactivateGroup(t, code, a, remember)
      )

    private val maxNodeLen = Grammar.reqCode.nodeLength.total.max

    /**
     * Rename a ReqCode so that the resulting code is available/free.
     *
     * To minimise disruption to other reqs and avoid further potential collisions, the resulting code will not exist
     * in the ReqCode trie at all (as opposed to selecting an existing node with inactive data).
     */
    def renameReqCodeToAvoidConflict(conflicted: Value, trie: Trie): Value = {
      val init     = conflicted.init
      val t        = NonEmptyVector.maybe(init, trie)(trie.dropPath)

      @tailrec
      def go(root: String, i: Int): Node = {
        val s = root + "_" + i
        if (s.length > maxNodeLen)
          go(root.init, 2)
        else {
          val n = Node(s)
          if (t hasValueK n)
            go(root, i + 1)
          else
            n
        }
      }

      val n = go(conflicted.last.value, 2)
      NonEmptyVector.end(init, n)
    }

    /**
     * Restore a requirement's inactive ReqCode back to active status.
     *
     * If the ReqCode is already active with another ID, then it has been usurped while it was inactive.
     * Usurped ReqCodes are renamed to avoid conflict before being restored.
     */
    def restoreCodeToReqT(trie: Trie, reqId: ReqId, id: ApReqCodeId, code: Value): Eval[Trie] =
      for {
        d <- needData(trie, code)
        _ <- ensureRefToReqExists(code, d, id)(reqId)
      } yield d match {

        case d: Inactive =>
          // ReqCode is available. Restore simply.
          val a = ActiveReq(id, reqId, d.deadGroup, d.reqInactive.del(reqId, id))
          trie.put(code, a)

        case _: ActiveGroup | _: ActiveReq =>
          // ReqCode has been usurped. Rename before restoration.
          val v2  = renameReqCodeToAvoidConflict(code, trie)
          val ri2 = emptyReqInactive.setvs(reqId, d.reqInactive(reqId) - id)
          val d2  = ActiveReq(id, reqId, None, ri2)
          trie
            .put(code, d.modReqInactive(_ delk reqId))
            .put(v2, d2)
      }

    /**
     * Restore a requirement's inactive ReqCodes back to active status.
     *
     * If more than one id refers to the same ReqCode, then only the id with the smallest value is activated.
     */
    def restoreToReqByIdsT(t0: Trie, reqId: ReqId, ids: Iterable[ApReqCodeId]): Eval[Trie] = {
      // Sort IDs here because only the first ID/reqcode is restored and we want determinism
      var idsSorted = ids.toVector
      if (idsSorted.length > 1)
        idsSorted = idsSorted.sorted

      needApCodes(idsSorted, ApReqCodeId.AndValue).flatMap { ivs =>
        var valuesSeen = Set.empty[Value]
        foldMapBind(t0, ivs)(iv => t =>
          if (valuesSeen contains iv.value)
            Eval.pure(t)
          else {
            valuesSeen += iv.value
            restoreCodeToReqT(t, reqId, iv.id, iv.value)
          }
        )
      }
    }

    def restoreBelongingToReqT(trie: Trie, reqId: ReqId): Eval[Trie] =
      restoreBelongingToReqsT(trie, set1(reqId))

    def restoreBelongingToReqsT(trie: Trie, reqIds: Set[ReqId]): Eval[Trie] =
      Eval.gets(_.content.reqCodes.inactiveIdsByReqId).flatMap(m =>
        foldMapBind(trie, reqIds)(reqId => restoreToReqByIdsT(_, reqId, m(reqId))))

    def restoreBelongingToReqs(reqIds: Set[ReqId]): Eval[Unit] =
      getTrie.flatMap(restoreBelongingToReqsT(_, reqIds)).flatMap(Project.reqCodeTrie.set)

    def restoreGroupAtCodeT(trie: Trie, id: ReqCodeId, code: Value): Eval[Trie] =
      needData(trie, code).flatMap {
        case d: Inactive =>
          // ReqCode is available. Restore simply.
          needDeadGroup(d, code).map { g =>
            val a = ActiveGroup(awakenGroup(g), d.reqInactive)
            trie.put(code, a)
          }

        case d: ActiveReq =>
          // ReqCode has been usurped. Rename before restoration.
          needDeadGroup(d, code).map { g =>
            val v2 = renameReqCodeToAvoidConflict(code, trie)
            val d2 = ActiveGroup(awakenGroup(g), emptyReqInactive)
            trie
              .put(code, d.copy(deadGroup = None))
              .put(v2, d2)
          }

        case _: ActiveGroup =>
          Eval.fail(s"Group at ${show(code)} is already live.")
      }

    def restoreGroupsByIdT(t0: Trie, ids: Iterable[ReqCodeGroupId]): Eval[Trie] =
      needCodeGroups(ids, (_, _)).flatMap(ivs =>
        foldMapBind(t0, ivs)(iv => t =>
          restoreGroupAtCodeT(t, iv._1, iv._2)))

    private def addCodesToReq(target: ReqId, mm: Multimap[ReqCode.Value, Set, ApReqCodeId]): Vector[AddReq] = {
      // Result order is important here
      val r = Vector.newBuilder[AddReq]
      mm.m.foreach { x =>
        val v    = x._1
        val ids1 = x._2
        if (ids1.size == 1) // TODO Scala 2.13 - use isSize or whatever
          r += AddReq(v, Unvalidated, ids1.head, target, true)
        else {
          // Sort IDs here because only the first ID becomes the ActiveReq.id and we want determinism
          val ids2 = ids1.toArray
          java.util.Arrays.sort(ids2, implicitly[Ordering[ApReqCodeId]])
          var first = true
          for (id <- ids2) {
            r += AddReq(v, Unvalidated, id, target, first)
            first = false
          }
        }
      }
      r.result()
    }

    def applyReqCodesPatch(e: ReqCodesPatch): Eval[Unit] =
      for {
        p    <- Eval.get
        refd = p.atomScan.codeRefs
        keep = e.add.values.foldLeft(refd)(_ -- _)
        t0   = p.content.reqCodes.trie
        t1   <- inactivateReqsByIdT(t0, e.remove, keep.contains, ensureActiveReqIs(e.id))
        t2   <- restoreToReqByIdsT(t1, e.id, e.restore)
        t3   <- addAllT(t2, addCodesToReq(e.id, e.add))
        _    <- Project.reqCodeTrie set t3
      } yield ()
  }

  // ===================================================================================================================
  object CodeGroupEvents {
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
