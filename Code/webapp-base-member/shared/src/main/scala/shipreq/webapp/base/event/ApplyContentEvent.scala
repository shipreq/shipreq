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
import SE.{SE, monadSE}

trait ApplyContentEvent {
  this: ApplyEvent =>

  object ContentCommon {
    val grIMap = IMapStore(Project.genericReqs)
    val ucIMap = IMapStore(Project.useCaseIMap)

    private val grLiveExplicitly = LiveAccessor(GenericReq.liveExplicitly)(_.id.toString)
    private val ucLiveExplicitly = LiveAccessor(UseCase.liveExplicitly)(_.id.toString)

    val updateReqIdCeiling = updateIdCeilingFn(IdCeilings.req)

    def ensureLiveReqId(reqId: ReqId): SE[Unit] =
      whenUntrusted(
        needReq(reqId) >>= ensureLiveReq)

    def ensureLiveReq(req: Req): SE[Unit] =
      whenUntrusted(
        SE.test(p => req.live(p.config.reqTypes) is Live, s"${show(req)} is dead."))

    def ensureLiveTextFieldId(id: CustomField.Text.Id): SE[Unit] =
      whenUntrusted(
        SE.getE(_.config.fields.customAttempt(id)) >>= ensureLiveTextField)

    def ensureLiveTextField(cf: CustomField.Text): SE[Unit] =
      SE.feed(p => ensureLive(cf live p.config)(show(cf)))

    def needReq[T <: ReqTypeId](reqId: ReqIdT[T]): SE[ReqT[T]] =
      reqId match {
        case id: GenericReqId => grIMap.need(id)
        case id: UseCaseId    => ucIMap.need(id)
      }

    private def deleteReq(id: ReqId): SE[Unit] =
      id match {
        case gr: GenericReqId => grIMap.update(gr, grLiveExplicitly.makeDead)
        case uc: UseCaseId    => ucIMap.update(uc, ucLiveExplicitly.makeDead)
      }

    private def restoreReq(id: ReqId): SE[Unit] =
      id match {
        case gr: GenericReqId => grIMap.update(gr, grLiveExplicitly.makeLive)
        case uc: UseCaseId    => ucIMap.update(uc, ucLiveExplicitly.makeLive)
      }

    /** Ignores reqcodes */
    def restore(reqIds: Iterable[ReqId]): SE[Unit] =
      SE.foldMapRun(reqIds)(restoreReq) // TODO Use one Project get/put

    def applyDelete(e: ReqsDelete): SE[Unit] = {
      val reqIds = e.reqs.whole
      for {
        _  <- SE.foldMapRun(reqIds)(deleteReq) // TODO Use one Project get/put
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

      NonEmptyVector.maybe(reason, noReason)(hasReason)
    }

    def validateTags(tagIds: => Iterable[ApplicableTagId]): SE[Unit] =
      whenUntrusted(
        SE.testO(p =>
        tagIds.iterator.map(p.config.tags.validateApplicableTag).find(_.isDefined) match {
          case Some(None) | None => None
          case Some(Some(err))   => Some(err)
        }
      ))

    def applyReqTagsPatch(e: ReqTagsPatch): SE[Unit] =
      ensureLiveReqId(e.id) >>
      validateTags(e.patch.value.allValues) >>
      Project.reqTags.modify(_.mod(e.id, e.patch.value.apply))

    def applyReqImplicationsPatch(e: ReqImplicationsPatch): SE[Unit] =
      ensureLiveReqId(e.id) >>
        Project.implicationsSrcToTgt.modify(is =>
          e.dir match {
            case Forwards  => e.patch.value.applyToMultimapValues(is)(e.id)
            case Backwards => e.patch.value.applyToMultimapKeys  (is)(e.id)
          }
        )

    def applyReqFieldCustomTextSet(e: ReqFieldCustomTextSet): SE[Unit] =
      ensureLiveReqId(e.id) >> setCustomTextValue(e.id, e.fid, e.value)

    def applyRestoreContent(e: ContentRestore): SE[Unit] =
      for {
        _  <- ContentCommon.restore(e.reqs)
        t1 <- ReqCodeLogic.getTrie
        t2 <- ReqCodeLogic.restoreBelongingToReqsT(t1, e.reqs)
        t3 <- ReqCodeLogic.restoreGroupsByIdT(t2, e.codeGroups)
        _  <- Project.reqCodeTrie set t3
      } yield ()

    def setCustomTextValue(id   : ReqId,
                           fid  : CustomField.Text.Id,
                           value: Text.CustomTextField.OptionalText): SE[Unit] =
      ensureLiveTextFieldId(fid) >> (Project.reqText ^|-> ReqData.textAt(fid, id)).set(value)

    def setCustomTextValueMap(id: ReqId,
                              values: NonEmpty[Map[CustomField.Text.Id, Text.CustomTextField.NonEmptyText]]): SE[Unit] =
      values.iterator
        .map { case (fid, value) => setCustomTextValue(id, fid, value.whole) }
        .reduce(_ >> _)

    def setReqCodes(id: ReqId, v: NonEmptySet[ApReqCodeId.AndValue]): SE[Unit] =
      ReqCodeLogic.addAllToReq(v.whole, id, addToActive = true)

    def setReqTags(id: ReqId, v: NonEmptySet[ApplicableTagId]): SE[Unit] =
      Project.reqTags.modify(_.addvs(id, v.whole))

    def setReqImpSrcs(id: ReqId, v: NonEmptySet[ReqId]): SE[Unit] =
      Project.implicationsSrcToTgt.modify(_.addks(v.whole, id))

    def setReqImpTgts(id: ReqId, v: NonEmptySet[ReqId]): SE[Unit] =
      Project.implicationsSrcToTgt.modify(_.addvs(id, v.whole))
  }

  // ===================================================================================================================

  object GenericReqEvents {
    import ContentCommon.{ucIMap => _, _}

    def ensureLiveCustomReqType(rt: CustomReqType): SE[Unit] =
      ensureLive(rt.live)(show(rt))

    def ensureLiveCustomReqTypeId(id: CustomReqTypeId): SE[Unit] =
      whenUntrusted(needCustomReqType(id) >>= ensureLiveCustomReqType)

    def needCustomReqType(id: CustomReqTypeId): SE[CustomReqType] =
      CustomReqTypeEvents.imap need id

    def needLiveCustomReqType(id: CustomReqTypeId): SE[CustomReqType] =
      needCustomReqType(id).flatTap(rt => ensureLive(rt.live)(rt.mnemonic.value))

    val applyGenericReqCreate: GenericReqCreate => SE[Unit] = {
      val ^ = GenericReqGD

      def foreachValue(id: GenericReqId, vs: ^.Values): SE[Unit] =
        SE.foldMapRun(vs.values) {
          case ^.ValueForCodes     (v) => setReqCodes          (id, v)
          case ^.ValueForCustomText(v) => setCustomTextValueMap(id, v)
          case ^.ValueForImpSrcs   (v) => setReqImpSrcs        (id, v)
          case ^.ValueForImpTgts   (v) => setReqImpTgts        (id, v)
          case ^.ValueForTags      (v) => setReqTags           (id, v)
          case ^.ValueForTitle     (_) => SE.nop // Handled below
        }

      @inline def emptyTitle: Text.GenericReqTitle.OptionalText =
        Vector.empty

      e => for {
        rt      ← needLiveCustomReqType(e.rt)
        reqData ← SE.get(_.content.reqs)
        id      = e.id
        title   = ^.Title.get(e.vs).fold(emptyTitle)(_.value.whole)
        pp      = reqData.pubids.allocC(rt.id)(id)
        req     = GenericReq(id, pp._2, title, Live)
        _       ← grIMap.create(req)
        _       ← Project.pubidRegister set pp._1
        _       ← foreachValue(id, e.vs)
        _       ← updateReqIdCeiling(id)
      } yield ()
    }

    def applyGenericReqTypeSet(e: GenericReqTypeSet): SE[Unit] =
      for {
        r <- grIMap.need(e.id)
        _ <- ensureLiveReq(r)
        _ <- ensureLiveCustomReqTypeId(e.value)
        _ <- Project.reqs.modify { reqs =>
               val pp = reqs.pubids.allocC(e.value)(e.id)
               val r2 = r.copy(pubid = pp._2)
               Requirements(reqs.genericReqs + r2, reqs.useCases, pp._1)
             }
      } yield ()

    def applyGenericReqTitleSet(e: GenericReqTitleSet): SE[Unit] =
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

    @inline def ensureStepExists(id: UseCaseStepId): SE[Unit] =
      ensureStepExistence(id, true)

    @inline def ensureStepDoesntExist(id: UseCaseStepId): SE[Unit] =
      ensureStepExistence(id, false)

    def ensureStepExistence(id: UseCaseStepId, expectToExist: Boolean): SE[Unit] =
      whenUntrusted(SE.test(
        _.content.reqs.useCases.stepIndex.contains(id) ==* expectToExist,
        show(id) + (if (expectToExist) " not found." else " already exists.")))

    def ensureStepsExist(_ids: => Traversable[UseCaseStepId]): SE[Unit] =
      whenUntrusted(SE.testO { p =>
        val si = p.content.reqs.useCases.stepIndex
        val ids = _ids
        if (ids forall si.contains)
          None
        else {
          val bad = ids.toSet -- si.keySet
          Some(bad.toList.map(_.value).sorted.mkString("Step(s) not found: {", ",","}."))
        }
      })

    def applyTitleSet(e: UseCaseTitleSet): SE[Unit] =
      ensureLiveReqId(e.id) >>
        ucIMap.updateF(e.id, _.copy(title = e.value))

    def postAddStep(ucId: UseCaseId, stepId: UseCaseStepId, field: StepField): SE[Unit] =
      postAddStep(ucId, stepId, field,
        (r, ucs) => r.copy(useCases = ucs),
        (ic, i) => ic.copy(useCaseStep = i))

    def postAddStep(ucId: UseCaseId, stepId: UseCaseStepId, field: StepField,
                    updReqs: (Requirements, UseCases) => Requirements,
                    updStepIdCeil: (IdCeilings, Int) => IdCeilings): SE[Unit] =
      SE.mod { p =>
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

    val applyCreate: UseCaseCreate => SE[Unit] = {
      val ^ = UseCaseGD

      def foreachValue(id: UseCaseId, vs: ^.Values): SE[Unit] =
        SE.foldMapRun(vs.values) {
          case ^.ValueForCodes     (v) => setReqCodes          (id, v)
          case ^.ValueForCustomText(v) => setCustomTextValueMap(id, v)
          case ^.ValueForImpSrcs   (v) => setReqImpSrcs        (id, v)
          case ^.ValueForImpTgts   (v) => setReqImpTgts        (id, v)
          case ^.ValueForTags      (v) => setReqTags           (id, v)
          case ^.ValueForTitle     (_) => SE.nop // Handled below
        }

      def postAdd(pr: PubidRegister, ucId: UseCaseId, stepId: UseCaseStepId): SE[Unit] =
        postAddStep(ucId, stepId, NCAC,
          (r, ucs) => r.copy(useCases = ucs, pubids = pr),
          (ic, i) => ic.copy(useCaseStep = i, req = ic.req max ucId))

      e => for {
        _       <- ensureStepDoesntExist(e.stepId)
        id      = e.id
        title   = ^.Title.get(e.vs).fold(Text.UseCaseTitle.empty)(_.value.whole)
        pp      ← SE get (_.content.reqs.pubids allocUC id)
        uc      = UseCase.empty(id, pp._2.pos, title, e.stepId)
        _       ← ucIMap.create(uc)
        _       ← postAdd(pp._1, id, e.stepId)
        _       ← foreachValue(id, e.vs)
      } yield ()
    }

    def applyStepCreate(e: UseCaseStepCreate): SE[Unit] = {
      val step = UseCaseStep(e.id, Text.UseCaseStep.empty, Live)
      val tree = e.field.useCaseStepTree

      def insert(loc: Location, uc: UseCase): SE[Unit] =
        tree.modifyF(_.insertAfter(loc, step))(uc) match {
          case Some(uc2) => ucIMap addOrUpdate uc2
          case None =>
            val ploc = VectorTree.PartialLocation(loc, Valid)
            val locStr = e.field.stepLabel(uc.pos, ploc, UseCaseStepLabelFmt.`N.m`)
            SE fail s"${show(step.id)} cannot be added to ${show(uc.id)} at location: $locStr"
        }

      def append(uc: UseCase): SE[Unit] =
        ucIMap addOrUpdate tree.modify(_ append step)(uc)

      for {
        _  ← ensureStepDoesntExist(step.id)
        uc ← ucIMap.need(e.ucId)
        _  ← ensureLiveReq(uc)
        _  ← e.at.location.fold(append(uc))(insert(_, uc))
        _  ← postAddStep(e.ucId, e.id, e.field)
      } yield ()
    }

    def needStepIndex(id: UseCaseStepId): SE[UseCases.StepTreeKey] =
      SE.get(_.content.reqs.useCases.stepIndex.get(id)) >>= (optionGet(_, s"${show(id)} not found."))

    def needStepFocus(id: UseCaseStepId): SE[UseCaseStep.Focus] =
      needStepIndex(id) >> SE.get(_.content.reqs.useCases.focusStep(id))

    private def findStepModTree(id: UseCaseStepId)(mod: (UseCaseSteps, StepField, Location, UseCaseStep) => SE[UseCaseSteps.Tree]): SE[Unit] =
      needStepIndex(id) >>= { idx =>
        val ucId = idx.useCaseId
        val f    = idx.field
        // It's fast to use findLoc below to find the step location rather than using some lazy index because
        // 1) The index would need to be recalculated every time a step in the tree is changed
        //    (meaning the index is only used once before being discarded).
        // 2) findLoc will stop when it finds the step. An index scans the whole tree.
        ucIMap.update(ucId, uc =>
          ensureLiveReq(uc) >>
            optionGet(f.useCaseSteps.get(uc).tree.findLocAndValue(_.id ==* id), s"${show(id)} not found.") >>=
              (ls => f.useCaseSteps.modifyF[SE](t => mod(t, f, ls._1, ls._2).map(UseCaseSteps(_)))(uc)))
      }

    private def mapStep(id: UseCaseStepId)(mod: UseCaseStep => UseCaseStep): SE[Unit] =
      findStepModTree(id)((steps, _, l, s) =>
        setStep(steps.tree, l)(mod(s)))

    private def setStep(t: UseCaseSteps.Tree, loc: Location)(s: UseCaseStep): SE[UseCaseSteps.Tree] =
      optionGet(t.modifyValueAt(loc)(_ => s), badStepIndex(s.id, loc))

    def applyStepShiftLeft(e: UseCaseStepShiftLeft): SE[Unit] =
      findStepModTree(e.id)((s, _, l, _) =>
        optionGet(s.tree shiftLeft l, s"${show(e.id)} cannot be shifted left."))

    def applyStepShiftRight(e: UseCaseStepShiftRight): SE[Unit] =
      findStepModTree(e.id)((s, _, l, _) =>
        optionGet(s.tree.shiftRightV(l, s.locValidity), s"${show(e.id)} cannot be shifted right."))

    private def badStepIndex(id: UseCaseStepId, loc: Location) =
      s"${show(id)} expected at ${showLoc(loc)}."

    val applyStepUpdate: UseCaseStepUpdate => SE[Unit] = {
      val ^ = UseCaseStepGD
      val GD = GenericDataApp[UseCaseStep](^)

      val noFlow = SetDiff.empty[UseCaseStepId]

      def getFlow(values: UseCaseStepGD.Values, a: UseCaseStepGD.Attr {type Data = SetDiff.NE[UseCaseStepId]}): SetDiff[UseCaseStepId] =
        a.get(values).fold(noFlow)(_.value)

      def updateFlow(id: UseCaseStepId, flow_← : SetDiff[UseCaseStepId], flow_→ : SetDiff[UseCaseStepId]): SE[Unit] =
        if (flow_←.isEmpty && flow_→.isEmpty)
          SE.nop
        else
          ensureStepsExist(flow_→.allValues ++ flow_←.allValues) >>
          StepFlowUni.modify { f0 =>
            var f = f0
            f = flow_→.applyToMultimapValues(f)(id)
            f = flow_←.applyToMultimapKeys(f)(id)
            f
          }

      def updateTitle(id: UseCaseStepId, title: Text.UseCaseStep.OptionalText): SE[Unit] =
        mapStep(id)(_.copy(titleExplicitly = title))

      e => {
        val gd       = e.vs.value
        def updFlow  = updateFlow(e.id, getFlow(gd, ^.FlowIn), getFlow(gd, ^.FlowOut))
        def updTitle = ^.Title.get(gd).fold(SE.nop)(v => updateTitle(e.id, v.value))

        ensureStepExists(e.id) >> updFlow >> updTitle
      }
    }

    def applyStepDelete(e: UseCaseStepDelete): SE[Unit] =
      for {
        f   <- needStepFocus(e.id)
        _   <- whenUntrusted(SE.test(f.field.canDelete(f.loc) is Allow, s"Deletion of step ${show(e.id)} forbidden."))
        del <- SE.get(_.deletionMethodForUseCaseStep(e.id))
        _   <- del match {
                 case DeletionMethod.Soft => setUseCaseStepLive(e.id, Dead)
                 case DeletionMethod.Hard => hardDeleteStep(f)
               }
      } yield ()

    private def hardDeleteStep(f: UseCaseStep.Focus): SE[Unit] =
      for {
        _ <- findStepModTree(f.id)((steps, _, loc, _) => SE.ret(steps.tree.remove(loc).get))
        _ <- postHardDelete(f.subtree)
      } yield ()

    private def postHardDelete(deleted: VectorTree.Node[UseCaseStep]): Project => Project =
      Project.useCases.modify { ucs =>
        def ids = deleted.valueIterator.map(_.id)
        val si2 = ucs.stepIndex -- ids
        ucs.copy(stepIndex = si2)
      }

    def applyStepRestore(e: UseCaseStepRestore): SE[Unit] =
      setUseCaseStepLive(e.id, Live)

    private def setUseCaseStepLive(id: UseCaseStepId, life: Live): SE[Unit] =
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

    val getTrie = SE get Project.reqCodeTrie.get

    def ensureInactive(d: Data, v: Value): SE[Unit] =
      whenUntrusted(d match {
        case _: Inactive    => SE.nop
        case _: ActiveReq
           | _: ActiveGroup => SE fail s"${show(v)} shouldn't be in use."
      })

    def ensureActiveReqIs(reqId: ReqId): ActiveReq => SE[Unit] =
      whenUntrusted(a => SE.test(a.reqId ==* reqId, s"Expected ReqCode target to be $reqId, found: ${a.reqId}."))

    def ensureRefToReqExists(v: Value, d: Data, rc: ApReqCodeId)(reqId: ReqId): SE[Unit] =
      whenUntrusted(
        SE.test(d.reqInactive(reqId) contains rc, s"Ref to ${show(reqId)} not found in ${show(v)}."))

    def needData(t: Trie, v: Value): SE[Data] =
      t.valueAtPath[SE[Data]](v, SE fail s"${show(v)} not found.")(SE.ret)

    def needActiveReq(d: Data, v: Value): SE[ActiveReq] =
      narrowCC[Data, ActiveReq](d, s"${show(v)} is not an ActiveReq.")

    def needActiveGroup(d: Data, v: Value): SE[ActiveGroup] =
      narrowCC[Data, ActiveGroup](d, s"${show(v)} is not an ActiveGroup.")

    def needDeadGroup(d: Data, v: Value): SE[DeadCodeGroup] =
      optionGet(d.deadGroup, s"Expected to find dead group at ${show(v)}.")

    def needCode(id: ReqCodeId): SE[Value] =
      SE(p => optionGetR(p, p.content.reqCodes.getReqCode(id), s"${show(id)} not found."))

    def needApCodes[A](ids: Traversable[ApReqCodeId], f: (ApReqCodeId, Value) => A): SE[Vector[A]] =
      needCodes(ids)(_.content.reqCodes.apReqCodesById.get, f)

    def needCodeGroups[A](ids: Traversable[ReqCodeGroupId], f: (ReqCodeGroupId, Value) => A): SE[Vector[A]] =
      needCodes(ids)(_.content.reqCodes.reqCodeGroupsById.get, f)

    def needCodes[I <: ReqCodeId, A](ids: Traversable[I])
                                    (getFn: Project => I => Option[Value], f: (I, Value) => A): SE[Vector[A]] = {
      SE { p =>
        val get = getFn(p)
        val found = Vector.newBuilder[A]
        var missing = Vector.empty[I]
        for (id <- ids)
          get(id) match {
            case Some(v) => found += f(id, v)
            case None    => missing :+= id
          }
        if (missing.nonEmpty)
          SE Failure s"Codes not found: ${missing.map(_.value).sorted.map("#" + _).mkString(", ")}"
        else
          SE.Ok(p, found.result())
      }
    }

    private def awakenGroup(g: DeadCodeGroup) = LiveCodeGroup(g.id, g.title)
    private def killGroup  (g: LiveCodeGroup) = DeadCodeGroup(g.id, g.title)

    sealed trait Adder[A] {
      def reqCodeId(a: A): ReqCodeId
      def apply(t: Trie, a: A): SE[Trie]
    }

    /** Command to add a ReqCode to a requirement. */
    case class AddReq(code: Value, codeValidated: Validated, id: ApReqCodeId, reqId: ReqId, addToActive: Boolean)

    implicit object ReqAdder extends Adder[AddReq] {
      override def reqCodeId(a: AddReq) = a.id

      override def apply(t: Trie, cmd: AddReq): SE[Trie] =
        cmd.codeValidated.mapValidated(cmd.code)(validateCode) { v =>
          type MakeNewData = SE[ActiveReq]
          import cmd.{addToActive, id, reqId}

          def createNode: MakeNewData =
            if (addToActive)
              SE ret ActiveReq(id, reqId, None, emptyReqInactive)
            else
              SE fail s"${show(v)} not found; can't add inactive ${show(id)} ."

          def modifyNode(d: Data): MakeNewData =
            if (addToActive)
              ensureInactive(d, v) |>>
                ActiveReq(id, reqId, d.deadGroup, d.reqInactive.del(reqId, id))
            else
              needActiveReq(d, v) |> (ar =>
                ar.copy(reqInactive = ar.reqInactive.add(reqId, id)))

          t.valueAtPath(v, createNode)(modifyNode).map(t.put(v, _))
        }
    }

    /** Command to add an active CodeGroup. */
    case class AddGroup(code: Value, codeValidated: Validated, g: LiveCodeGroup)

    implicit object GroupAdder extends Adder[AddGroup] {
      override def reqCodeId(a: AddGroup) = a.g.id

      override def apply(t: Trie, cmd: AddGroup): SE[Trie] =
        cmd.codeValidated.mapValidated(cmd.code)(validateCode) { v =>
          type MakeNewData = SE[ActiveGroup]
          import cmd.g

          def createNode: MakeNewData =
            SE ret ActiveGroup(g, emptyReqInactive)

          def modifyNode(d: Data): MakeNewData =
            ensureInactive(d, v) |>>
              ActiveGroup(g, d.reqInactive)

          t.valueAtPath(v, createNode)(modifyNode).map(t.put(v, _))
        }
    }

    def addOne[A](a: A)(implicit adder: Adder[A]): SE[Unit] =
      getTrie >>= (addOneT(_, a)) >>= Project.reqCodeTrie.set

    def addAll[A](as: Iterable[A])(implicit adder: Adder[A]): SE[Unit] =
      getTrie >>= (addAllT(_, as)) >>= Project.reqCodeTrie.set

    def addOneT[A](t: Trie, a: A)(implicit adder: Adder[A]): SE[Trie] =
      updateIdCeiling(adder.reqCodeId(a).value) >>
        adder(t, a)

    def addAllT[A](t: Trie, as: Iterable[A])(implicit adder: Adder[A]): SE[Trie] =
      updateIdCeiling(IdCeilings.maxOfF(as)(adder.reqCodeId(_).value)) >>
        foldMapBind(t, as)(a => adder(_, a))

    def addAllToReq(vs: Iterable[ApReqCodeId.AndValue], reqId: ReqId, addToActive: Boolean): SE[Unit] =
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

    def inactivateReqsByCodeT(t: Trie, codes: Iterable[Value], remember: ReqCodeId => Boolean, validateTarget: ActiveReq => SE[Unit]): SE[Trie] =
      foldMapBind(t, codes)(v => t =>
        for {
          d <- needData(t, v)
          a <- needActiveReq(d, v)
          _ <- validateTarget(a)
        } yield inactivateReq(t, v, a, remember(a.id))
      )

    def inactivateReqsByIdT(t: Trie, ids: Traversable[ApReqCodeId], remember: ReqCodeId => Boolean, validateTarget: ActiveReq => SE[Unit]): SE[Trie] =
      needApCodes(ids, (_, v) => v) >>= (vs =>
        inactivateReqsByCodeT(t, vs, remember, validateTarget))

    def inactivateBelongingToReqs(reqIds: Set[ReqId]): SE[Unit] =
      getTrie >>= (inactivateBelongingToReqsT(_, reqIds)) >>= Project.reqCodeTrie.set

    def inactivateBelongingToReqsT(trie: Trie, reqIds: Set[ReqId]): SE[Trie] =
      SE.get(_.content.reqCodes.activeReqCodesByReqId) >>= (m =>
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

    def inactivateGroupsByIdT(t: Trie, ids: Traversable[ReqCodeGroupId], remember: Boolean): SE[Trie] =
      needCodeGroups(ids, (_, v) => v) >>= (vs =>
        inactivateGroupsByCodeT(t, vs, remember))

    def inactivateGroupsByCodeT(t: Trie, codes: Iterable[Value], remember: Boolean): SE[Trie] =
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
    def restoreCodeToReqT(trie: Trie, reqId: ReqId, id: ApReqCodeId, code: Value): SE[Trie] =
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
    def restoreToReqByIdsT(t0: Trie, reqId: ReqId, ids: Iterable[ApReqCodeId]): SE[Trie] = {
      // Sort IDs here because only the first ID/reqcode is restored and we want determinism
      var idsSorted = ids.toVector
      if (idsSorted.length > 1)
        idsSorted = idsSorted.sorted

      needApCodes(idsSorted, ApReqCodeId.AndValue) >>= { ivs =>
        var valuesSeen = Set.empty[Value]
        foldMapBind(t0, ivs)(iv => t =>
          if (valuesSeen contains iv.value)
            SE ret t
          else {
            valuesSeen += iv.value
            restoreCodeToReqT(t, reqId, iv.id, iv.value)
          }
        )
      }
    }

    def restoreBelongingToReqT(trie: Trie, reqId: ReqId): SE[Trie] =
      restoreBelongingToReqsT(trie, set1(reqId))

    def restoreBelongingToReqsT(trie: Trie, reqIds: Set[ReqId]): SE[Trie] =
      SE.get(_.content.reqCodes.inactiveIdsByReqId) >>= (m =>
        foldMapBind(trie, reqIds)(reqId => restoreToReqByIdsT(_, reqId, m(reqId))))

    def restoreBelongingToReqs(reqIds: Set[ReqId]): SE[Unit] =
      getTrie >>= (restoreBelongingToReqsT(_, reqIds)) >>= Project.reqCodeTrie.set

    def restoreGroupAtCodeT(trie: Trie, id: ReqCodeId, code: Value): SE[Trie] =
      needData(trie, code) >>= {
        case d: Inactive =>
          // ReqCode is available. Restore simply.
          needDeadGroup(d, code) |> { g =>
            val a = ActiveGroup(awakenGroup(g), d.reqInactive)
            trie.put(code, a)
          }

        case d: ActiveReq =>
          // ReqCode has been usurped. Rename before restoration.
          needDeadGroup(d, code) |> { g =>
            val v2 = renameReqCodeToAvoidConflict(code, trie)
            val d2 = ActiveGroup(awakenGroup(g), emptyReqInactive)
            trie
              .put(code, d.copy(deadGroup = None))
              .put(v2, d2)
          }

        case _: ActiveGroup =>
          SE fail s"Group at ${show(code)} is already live."
      }

    def restoreGroupsByIdT(t0: Trie, ids: Iterable[ReqCodeGroupId]): SE[Trie] =
      needCodeGroups(ids, (_, _)) >>= (ivs =>
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

    def applyReqCodesPatch(e: ReqCodesPatch): SE[Unit] =
      for {
        p    ← SE.get
        refd = p.atomScan.codeRefs
        keep = e.add.values.foldLeft(refd)(_ -- _)
        t0   = p.content.reqCodes.trie
        t1   ← inactivateReqsByIdT(t0, e.remove, keep.contains, ensureActiveReqIs(e.id))
        t2   ← restoreToReqByIdsT(t1, e.id, e.restore)
        t3   ← addAllT(t2, addCodesToReq(e.id, e.add))
        _    ← Project.reqCodeTrie set t3
      } yield ()
  }

  // ===================================================================================================================
  object CodeGroupEvents {
    import ReqCodeLogic._

    val ^ = CodeGroupGD
    val GD = GenericDataApp[ReqCodes](^)

    def applyCreate(e: CodeGroupCreate): SE[Unit] = {
      implicit val vs = e.vs
      for {
        c ← GD.need(^.Code)
        t = GD.want(^.Title)(Vector.empty)
        g = LiveCodeGroup(e.id, t)
        _ ← addOne(AddGroup(c, Unvalidated, g))
      } yield ()
    }

    private def updateGroupCode(id: ReqCodeId, newCode: ReqCode.Value): SE[Unit] =
      for {
        t  ← getTrie
        v  ← needCode(id)
        d  ← needData(t, v)
        ag ← needActiveGroup(d, v)
        t2 = inactivateGroup(t, v, ag, false)
        t3 ← addOneT(t2, AddGroup(newCode, Unvalidated, ag.group))
        _  ← Project.reqCodeTrie set t3
      } yield ()

    private def modifyGroup(id: ReqCodeId, f: LiveCodeGroup => LiveCodeGroup): SE[Unit] =
      for {
        t  ← getTrie
        v  ← needCode(id)
        d  ← needData(t, v)
        ag ← needActiveGroup(d, v)
        d2 = ReqCode.ActiveGroup.group.modify(f)(ag)
        t2 = t.put(v, d2)
        _  ← Project.reqCodeTrie set t2
      } yield ()

    def applyUpdate(e: CodeGroupUpdate): SE[Unit] =
      SE.foldMapRun(e.vs.values) {
        case ^.ValueForTitle(t) => modifyGroup(e.id, _.copy(title = t))
        case ^.ValueForCode (v) => updateGroupCode(e.id, v)
      }

    def applyDelete(e: CodeGroupsDelete): SE[Unit] =
      getTrie >>= (inactivateGroupsByIdT(_, e.ids.whole, true)) >>= Project.reqCodeTrie.set
  }
}
