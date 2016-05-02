package shipreq.webapp.base.event

import nyaya.gen._
import nyaya.prop.LogicPropExt
import nyaya.util.Multimap
import scalaz.{-\/, BindRec}
import shipreq.base.test.BaseUtilGen._
import shipreq.base.test.IncCounter
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.RandomData
import shipreq.webapp.base.data._
import shipreq.webapp.base.hash._
import shipreq.webapp.base.test.DataTestExt._
import shipreq.webapp.base.test.WebappBaseGen._
import shipreq.webapp.base.text.Text
import ApplicableEventGen.ObserveFn
import RandomData.{fieldRefKey, hashRefKey, implicationRequired, mandatory, mutexChildren}
import RandomData.{reqCode, reqTypeMnemonic, TextGen, TextGenExt, unicodeString1}
import ScalaExt._
import UtilMacros._

/**
  * Generates a random event stream that can be successfully applied.
  *
  * This differs from the events that [[RandomData]] can generate which are only valid in isolation and often don't
  * make sense as a consecutive stream.
  */
object RandomEventStream {
//  def applicableEventS[S](observe: ObserveFn[S]): StateGen[(S, Project), Event] =
//    StateGen(sp =>
//      ApplicableEventGen(sp._2).applicableEventS(sp._1)(observe))
//
//  def addVerifiedEventS[S](succ: StateGen[(S, Project), Event]): StateGen[((S, Project), VerifiedEvents), VerifiedEvent] =
//    StateGen { case orig @ (sp1, vs) =>
//      succ(sp1).map { case (sp2, e2) =>
//        val hrs = HashRec.changes(sp1._2, sp2._2)
//        val v2 = VerifiedEvent(e2, hrs)
//        ((sp2, vs :+ v2), v2)
//      }
//    }
//
//  def eventStreamS[S](gen: StateGen[((S, Project), VerifiedEvents), VerifiedEvent])(implicit ss: SizeSpec): StateGen[((S, Project), VerifiedEvents), Unit] = {
//    val SSS = scalaz.StateT.stateTMonadState[((S, Project), VerifiedEvents), Gen]
//    import SSS.monadSyntax._
//    // TO DO Speed up replicateM_ (?)
//    StateGen(spv => ss.gen.flatMap(n => gen.replicateM_(n)(spv)))
//  }
//
//  def genEventStreamS[S](s0: S, p0: Project = Project.empty)(observe: ObserveFn[S])(implicit ss: SizeSpec): Gen[(S, VerifiedEvents)] =
//    eventStreamS(addVerifiedEventS(applicableEventS(observe)))(ss)((s0, p0), Vector.empty)
//      .map { case (((s, _), vs), ()) => (s, vs) }
//
//  // -------------------------------------------------------------------------------------------------------------------
//
//  def withEventStats(p: Project = Project.empty)(implicit ss: SizeSpec): Gen[(EventStats, VerifiedEvents)] =
//    genEventStreamS(EventStats.empty, p)(EventStats.observeFn)
}

// =====================================================================================================================

object ApplicableEventGen {
  @inline def apply(p: Project) = new ApplicableEventGen(p)

  type ObserveFn[S] = (S, Event, ApplyEvent.Result) => S
}

class ApplicableEventGen(p: Project) {
  private implicit val gss: SizeSpec = 0 to 3

  // If the starting state isn't valid, event application will never succeed and thus, loop forever
  if (p ne Project.empty)
    DataProp.project.allIncludingConfig assert p

  private val cfg = p.config

  private implicit def autoGenToOptionGen[A](g: Gen[A]): Option[Gen[A]] = Some(g)

  private def deletionAction(l: Live): DeletionAction =
    if (l :: Live) Delete else Restore

  val (staticFieldsToDel, staticFieldsToAdd) =
    StaticField.values.whole.partition(cfg.fields.staticFieldSet.contains)
      .map1(_.filter(_.deletable :: Deletable))

  val nextReqId: Gen[Int] =
    IncCounter genInt p.idCeilings.req

  val nextGenericReqId: Gen[GenericReqId] =
    nextReqId map GenericReqId

  val nextReqCodeId: Gen[ReqCodeId] =
    IncCounter genInt p.idCeilings.reqCode map ReqCodeId

  val nextCustomIssueTypeId: Gen[CustomIssueTypeId] =
    IncCounter genInt p.idCeilings.customIssueType map CustomIssueTypeId

  val nextCustomReqTypeId: Gen[CustomReqTypeId] =
    IncCounter genInt p.idCeilings.customReqType map CustomReqTypeId

  val nextCustomField: Gen[Int] =
    IncCounter genInt p.idCeilings.customField

  val nextCustomFieldImplicationId: Gen[CustomField.Implication.Id] =
    nextCustomField map CustomField.Implication.Id

  val nextCustomFieldTagId: Gen[CustomField.Tag.Id] =
    nextCustomField map CustomField.Tag.Id

  val nextCustomFieldTextId: Gen[CustomField.Text.Id] =
    nextCustomField map CustomField.Text.Id

  val nextTagId: Gen[Int] =
    IncCounter genInt p.idCeilings.tag

  val nextApplicableTagId: Gen[ApplicableTagId] =
    nextTagId map ApplicableTagId

  val nextTagGroupId: Gen[TagGroupId] =
    nextTagId map TagGroupId

  val nextUseCaseId: Gen[UseCaseId] =
    nextReqId map UseCaseId

  val nextUseCaseStepId: Gen[UseCaseStepId] =
    IncCounter genInt p.idCeilings.useCaseStep map UseCaseStepId

  lazy val liveTagId: Option[Gen[TagId]] =
    Gen.tryGenChoose(p.config.tags.valuesIterator.map(_.tag).filter(_.live :: Live).map(_.id))

  def liveTagGroupId: Option[Gen[TagGroupId]] =
    Gen.tryGenChoose(p.config.tags.valuesIterator.map(_.tag).filter(_.live :: Live).filterT[TagGroup].map(_.id))

  lazy val liveApplicableTagId: Option[Gen[ApplicableTagId]] =
    Gen.tryGenChoose(p.config.tags.valuesIterator.map(_.tag).filter(_.live :: Live).filterT[ApplicableTag].map(_.id))

  lazy val existingApplicableTagId: Option[Gen[ApplicableTagId]] =
    Gen.tryGenChoose(p.config.tags.keysIterator.filterT[ApplicableTagId])

  def tagChildren: Gen[TagInTree.Children] =
    liveTagId match {
      case Some(g) => g.set.map(_.toVector)
      case None    => Gen pure Vector.empty
    }

  def tagParents: Gen[TagInTree.Parents] =
    liveTagId match {
      case Some(g) => g.option.mapBy(g)
      case None    => Gen pure Map.empty
    }

  lazy val liveCustomReqTypeId: Option[Gen[CustomReqTypeId]] =
    Gen.tryGenChoose(cfg.reqTypes.custom.valuesIterator.filter(_.live :: Live).map(_.id))

  lazy val applicableReqTypes: Gen[Field.ApplicableReqTypes] =
    RandomData.applicableReqTypes(cfg.reqTypes.custom.keySet)

  lazy val existingReqId: Option[Gen[ReqId]] =
    Gen.tryGenChoose(p.reqs.reqs.keysIterator)

  lazy val liveReqIds: Vector[ReqId] =
    p.reqs.reqIterator.filter(_.live(cfg.reqTypes) :: Live).map(_.id).toVector

  lazy val liveReqId: Option[Gen[ReqId]] =
    Gen.tryGenChoose(liveReqIds)

  lazy val liveGenericReqId: Option[Gen[GenericReqId]] =
    Gen.tryGenChoose(p.reqs.genericReqs.valuesIterator.filter(_.live(cfg.reqTypes) :: Live).map(_.id))

  def liveUseCaseIterator: Iterator[UseCase] =
    p.reqs.useCases.imap.valuesIterator.filter(_.liveUC :: Live)

  lazy val liveUseCase: Option[Gen[UseCase]] =
    Gen.tryGenChoose(liveUseCaseIterator)

  lazy val liveUseCaseId: Option[Gen[UseCaseId]] =
    liveUseCase.map(_.map(_.id))

  lazy val existingUseCaseStepId: Option[Gen[UseCaseStepId]] =
    Gen.tryGenChoose(p.reqs.useCases.stepIterator.map(_.id))

  lazy val existingReqCodeId: Option[Gen[ReqCodeId]] =
    Gen.tryGenChoose(p.reqCodes.idList)

  lazy val liveReqCodeGroupId: Option[Gen[ReqCodeId]] =
    Gen.tryGenChoose(p.reqCodes.groups.iterator.filter(_.live :: Live).map(_.id).toVector)

  lazy val deadReqCodeGroupId: Option[Gen[ReqCodeId]] =
    Gen.tryGenChoose(p.reqCodes.groups.iterator.filter(_.live :: Dead).map(_.id).toVector)

  lazy val existingCustomIssueTypeId: Option[Gen[CustomIssueTypeId]] =
    Gen.tryGenChoose(cfg.customIssueTypes.keySet)

  lazy val liveCustomIssueTypeId: Option[Gen[CustomIssueTypeId]] =
    Gen.tryGenChoose(cfg.customIssueTypes.valuesIterator.filter(_.live :: Live).map(_.id))

  def liveCustomFieldId: Option[Gen[CustomFieldId]] =
    Gen.tryGenChoose(cfg.fields.customFields.valuesIterator.filter(_.live(cfg) :: Live).map(_.id))

  def liveCustomFieldImpId: Option[Gen[CustomField.Implication.Id]] =
    Gen.tryGenChoose(cfg.customImpFields.filter(_.live(cfg) :: Live).map(_.id))

  def liveCustomFieldTagId: Option[Gen[CustomField.Tag.Id]] =
    Gen.tryGenChoose(cfg.customTagFields.filter(_.live(cfg) :: Live).map(_.id))

  lazy val liveCustomFieldTextId: Option[Gen[CustomField.Text.Id]] =
    Gen.tryGenChoose(cfg.customTextFields.filter(_.live(cfg) :: Live).map(_.id))

  def customTextFieldText =
    TextGen.customTextFieldAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingCustomIssueTypeId, existingApplicableTagId).text

  lazy val newReqCodeIdAndValue =
    Gen.apply2(ReqCode.IdAndValue)(nextReqCodeId, reqCode.value)

  def reqCodeGroupTitle =
    TextGen.reqCodeGroupTitleAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingCustomIssueTypeId).text

  private lazy val genericReqTitleAtom =
    TextGen.genericReqTitleAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingCustomIssueTypeId, existingApplicableTagId)

  def genericReqTitle =
    genericReqTitleAtom.text

  def genericReqTitle1 =
    genericReqTitleAtom.text1(Text.GenericReqTitle)

  def deletionReason =
    TextGen.deletionReasonAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingApplicableTagId).text

  lazy val useCaseTitleAtom =
    TextGen.useCaseTitleAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingCustomIssueTypeId, existingApplicableTagId)

  def useCaseTitle =
    useCaseTitleAtom.text

  def useCaseTitle1 =
    useCaseTitleAtom.text1(Text.UseCaseTitle)

  def useCaseStepTitle =
    TextGen.useCaseStepAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingCustomIssueTypeId, existingApplicableTagId).text

  object customIssueTypeGD extends GenericDataGen(CustomIssueTypeGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Key  => hashRefKey            map Key .apply
      case Desc => unicodeString1.option map Desc.apply
    }
  }

  object customReqTypeGD extends GenericDataGen(CustomReqTypeGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Name        => unicodeString1      map Name       .apply
      case Imp         => implicationRequired map Imp        .apply
      case gd.Mnemonic => reqTypeMnemonic     map gd.Mnemonic.apply
    }
  }

  object customTextFieldGD extends GenericDataGen(CustomTextFieldGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Name      => unicodeString1     map Name     .apply
      case Key       => fieldRefKey        map Key      .apply
      case Mandatory => mandatory          map Mandatory.apply
      case ReqTypes  => applicableReqTypes map ReqTypes .apply
    }
  }

  object customTagFieldGD extends GenericDataOptionGen(CustomTagFieldGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case TagId     => liveTagId     map (_ map TagId    .apply)
      case Mandatory => mandatory            map Mandatory.apply
      case ReqTypes  => applicableReqTypes   map ReqTypes .apply
    }
  }

  object customImpFieldGD extends GenericDataOptionGen(CustomImpFieldGD) {
    import gd._

    private def reqTypesUsedInFields: Set[ReqTypeId] =
      cfg.customImpFields.map(_.reqTypeId).toSet

    private def liveReqTypes: Iterator[ReqTypeId] =
      StaticReqType.values.iterator.map(_.reqTypeId) ++
      cfg.reqTypes.custom.valuesIterator.filter(_.live :: Live).map(_.id)

    private def reqTypeId: Option[Gen[ReqTypeId]] =
      Gen.tryGenChoose(liveReqTypes.filterNot(reqTypesUsedInFields.contains))

    override def valueFor(a: Attr) = a match {
      case ReqTypeId => reqTypeId          map (_ map ReqTypeId.apply)
      case Mandatory => mandatory                 map Mandatory.apply
      case ReqTypes  => applicableReqTypes        map ReqTypes .apply
    }
  }

  object createGenericReqGD extends GenericDataOptionGen(CreateGenericReqGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Title    => genericReqTitle1                map Title   .apply
      case ReqCodes => newReqCodeIdAndValue       .nes map ReqCodes.apply
      case Tags     => liveApplicableTagId  map (_.nes map Tags    .apply)
      case ImpSrcs  => liveReqId            map (_.nes map ImpSrcs .apply)
      case ImpTgts  => liveReqId            map (_.nes map ImpTgts .apply)
    }
  }

  object reqCodeGroupGD extends GenericDataGen(ReqCodeGroupGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Code  => reqCode.value     map Code .apply
      case Title => reqCodeGroupTitle map Title.apply
    }
  }

  object applicableTagGD extends GenericDataGen(ApplicableTagGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Name     => unicodeString1        map Name    .apply
      case Desc     => unicodeString1.option map Desc    .apply
      case Key      => hashRefKey            map Key     .apply
      case Children => tagChildren           map Children.apply
      case Parents  => tagParents            map Parents .apply
    }
  }

  object tagGroupGD extends GenericDataGen(TagGroupGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Name          => unicodeString1        map Name         .apply
      case Desc          => unicodeString1.option map Desc         .apply
      case MutexChildren => mutexChildren         map MutexChildren.apply
      case Children      => tagChildren           map Children     .apply
      case Parents       => tagParents            map Parents      .apply
    }
  }

  object createUseCaseGD extends GenericDataOptionGen(CreateUseCaseGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Title    => useCaseTitle1                   map Title   .apply
      case ReqCodes => newReqCodeIdAndValue       .nes map ReqCodes.apply
      case Tags     => liveApplicableTagId  map (_.nes map Tags    .apply)
      case ImpSrcs  => liveReqId            map (_.nes map ImpSrcs .apply)
      case ImpTgts  => liveReqId            map (_.nes map ImpTgts .apply)
    }
  }

  def useCaseStepGD(uc: UseCase, step: UseCaseStep): Gen[UseCaseStepGD.NonEmptyValues] = {
    import UseCaseStepGD._

    lazy val gSteps =
      Gen.choose_!(uc.stepIterator.map(_.id)).set

    def gFlow(dir: Direction) =
      gSteps.map(ss => NonEmpty(SetDiff.xor(p.reqs.useCases.stepFlow(dir)(step.id), ss)))

    Gen { c =>
      var vs = emptyValues

      if (c.nextBit()) {
        if (c.nextBit()) gFlow(Forwards ).run(c).foreach(vs += FlowOut(_))
        if (c.nextBit()) gFlow(Backwards).run(c).foreach(vs += FlowIn (_))
      }

      if (vs.isEmpty || c.nextBit())
        vs += Title(useCaseStepTitle run c)

      NonEmpty force vs
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  def addStaticField: Option[Gen[AddStaticField]] =
    Gen.tryGenChoose(staticFieldsToAdd).map(_ map AddStaticField)

  def addUseCaseStep: Option[Gen[AddUseCaseStep]] =
    liveUseCase.map(genUC =>
      for {
        id    ← nextUseCaseStepId
        uc    ← genUC
        field ← Gen choose_! StaticField.useCaseStepTrees.whole.filter(_.useCaseStepTree.get(uc).nonEmpty)
        tree  = field.useCaseStepTree.get(uc)
        loc   ← tree.genParentLocation
      } yield AddUseCaseStep(id, uc.id, field, loc)
    )

  def applyTemplate: Option[Gen[ApplyTemplate]] =
    if (p eq Project.empty)
      Some(RandomData.events.applyTemplate)
    else
      None

  def createApplicableTag: Gen[CreateApplicableTag] =
    Gen.apply2(CreateApplicableTag)(nextApplicableTagId, applicableTagGD.allValues)

  def createCustomImpField: Option[Gen[CreateCustomImpField]] =
    customImpFieldGD.allValues.map(vs =>
      Gen.apply2(CreateCustomImpField)(nextCustomFieldImplicationId, vs))

  def createCustomIssueType: Gen[CreateCustomIssueType] =
    Gen.apply2(CreateCustomIssueType)(nextCustomIssueTypeId, customIssueTypeGD.allValues)

  def createCustomReqType: Gen[CreateCustomReqType] =
    Gen.apply2(CreateCustomReqType)(nextCustomReqTypeId, customReqTypeGD.allValues)

  def createCustomTagField: Option[Gen[CreateCustomTagField]] =
    customTagFieldGD.allValues.map(vs =>
      Gen.apply2(CreateCustomTagField)(nextCustomFieldTagId, vs))

  def createCustomTextField: Gen[CreateCustomTextField] =
    Gen.apply2(CreateCustomTextField)(nextCustomFieldTextId, customTextFieldGD.allValues)

  def createGenericReq: Option[Gen[CreateGenericReq]] =
    for {
      reqTypeId <- liveCustomReqTypeId
      //vs        <- createGenericReqGD.allPossibleValues
    } yield
      Gen.apply3(CreateGenericReq)(nextGenericReqId, reqTypeId, createGenericReqGD.values)

  def createReqCodeGroup: Gen[CreateReqCodeGroup] =
    Gen.apply2(CreateReqCodeGroup)(nextReqCodeId, reqCodeGroupGD.allValues)

  def createTagGroup: Gen[CreateTagGroup] =
    Gen.apply2(CreateTagGroup)(nextTagGroupId, tagGroupGD.allValues)

  def createUseCase: Gen[CreateUseCase] =
    Gen.apply3(CreateUseCase)(nextUseCaseId, nextUseCaseStepId, createUseCaseGD.values)

  def deleteCustomField: Option[Gen[DeleteCustomField]] =
    liveCustomFieldId.map(_.map(id =>
      DeleteCustomField(id, deletionAction(cfg.fields.customFields.need(id) live cfg))))

  def deleteCustomIssueType: Option[Gen[DeleteCustomIssueType]] =
    liveCustomIssueTypeId.map(_.map(id =>
      DeleteCustomIssueType(id, deletionAction(cfg.customIssueTypes.need(id).live))))

  def deleteCustomReqType: Option[Gen[DeleteCustomReqType]] =
    liveCustomReqTypeId.map(_.map(id =>
      DeleteCustomReqType(id, deletionAction(cfg.reqTypes.need(id).live))))

  def deleteReqCodeGroups: Option[Gen[DeleteReqCodeGroups]] =
    liveReqCodeGroupId.map(_.nes map DeleteReqCodeGroups)

  def deleteReqs: Option[Gen[DeleteReqs]] =
    liveReqId.map(reqId =>
      Gen.apply3(DeleteReqs)(reqId.nes, liveReqCodeGroupId.setE, deletionReason))

  def deleteStaticField: Option[Gen[DeleteStaticField]] =
    Gen.tryGenChoose(staticFieldsToDel).map(_ map DeleteStaticField)

  def deleteTag: Option[Gen[DeleteTag]] =
    liveTagId.map(g =>
      g.map(id =>
        DeleteTag(id, deletionAction(cfg.tags.get(id).fold[Live](Live)(_.tag.live)))))

  def deleteUseCaseStep: Option[Gen[DeleteUseCaseStep]] = {
    val ids = p.useCaseStepsDeletable.map(_.id)
    Gen.tryGenChooseLazily(ids).map(_ map DeleteUseCaseStep)
  }

  def restoreUseCaseStep: Option[Gen[RestoreUseCaseStep]] = {
    val ids = p.useCaseStepsRestorable.map(_.id)
    Gen.tryGenChooseLazily(ids).map(_ map RestoreUseCaseStep)
  }

  private def patchImplications[A](cmd: (ReqId, SetDiff.NE[ReqId]) => A, dir: Direction): Option[Gen[A]] =
    if (liveReqIds.length < 2)
      None
    else
      liveReqId.map { gReqId =>
        gReqId.flatMap { id =>
          Gen.choose_!(liveReqIds.filter(_ !=* id)).set1.map { ids =>
            val sd = SetDiff.xor(p.implications(dir)(id), ids)
            cmd(id, NonEmpty force sd)
          }
        }
      }

  def patchImplicationSrc: Option[Gen[PatchImplicationSrc]] =
    patchImplications(PatchImplicationSrc, Backwards)

  def patchImplicationTgt: Option[Gen[PatchImplicationTgt]] =
    patchImplications(PatchImplicationTgt, Forwards)

  def patchReqCodes: Option[Gen[PatchReqCodes]] =
    liveReqId.map(gReqId =>
      for {
        reqId          ← gReqId
        inactiveValues = p.reqCodes.inactiveIdsByReqId(reqId)
        restore        ← Gen.tryGenChoose(inactiveValues.toVector).setE(0 to 2)
        activeValues   = p.reqCodes.activeReqCodesByReqId(reqId)
        activeIds      = activeValues.map(p.reqCodes(_).activeId.get)
        remove         ← Gen.tryGenChoose(activeIds.toVector).setE(0 to 2)
        renameIds      ← Gen.tryGenChoose(remove.toVector).setE(0 to 2)
        addMin         = if (remove.nonEmpty || restore.nonEmpty) 0 else 1
        addIds         ← nextReqCodeId.list(addMin to 2)
        add            ← Gen sequence (addIds ++ renameIds).map(id => reqCode.value.strengthR(Set.empty[ReqCodeId] + id))
      } yield
        PatchReqCodes(reqId, remove, restore, Multimap(add.toMap))
    )

  def patchReqTags: Option[Gen[PatchReqTags]] =
    for {
      gId  <- liveReqId
      gTag <- existingApplicableTagId
    } yield for {
      id   <- gId
      tags <- gTag.nes(1 to 5, implicitly)
    } yield {
      val sd = SetDiff.xor(p.reqTags(id), tags.whole)
      PatchReqTags(id, NonEmpty force sd)
    }

  def repositionField: Gen[RepositionField] =
    Gen.lazily {
      val order = p.config.fields.order
      // if (order.length < 2) // We always have at least 2 because of static fields
      val anyField = Gen.choose_!(order)
      for {
        id     ← anyField
        curPos = RelPos.get(order, id)
        tmp    = order.filterNot(f => (f ==* id) || curPos.exists(f ==* _)).map(_.some)
        tmp2   = if (curPos.isEmpty) tmp else tmp :+ None
        pos    ← Gen.choose_!(tmp2)
      } yield RepositionField(id, pos)
    }

  def restoreContent: Option[Gen[RestoreContent]] = {
    val restorableReqIds = Gen.tryGenChoose[ReqId](
      p.reqs.reqIterator.filter {
        case g: GenericReq => (g.liveExplicitly :: Dead) && (g.copy(liveExplicitly = Live).live(cfg.reqTypes) :: Live)
        case u: UseCase    => (u.liveExplicitly :: Dead) && (u.copy(liveExplicitly = Live).live(cfg.reqTypes) :: Live)
      }.map(_.id).toVector)
    if (restorableReqIds.isEmpty && deadReqCodeGroupId.isEmpty)
      None
    else Some {
      val idSet = restorableReqIds.setE
      val codeSet = deadReqCodeGroupId.setE
      Gen.apply2(RestoreContent)(idSet, codeSet).flatMap(cmd =>
        if (cmd.reqs.nonEmpty || cmd.reqCodeGroups.nonEmpty)
          Gen pure cmd
        else if (restorableReqIds.isDefined)
          restorableReqIds.get.nes.map(ids => RestoreContent(ids.whole, Set.empty))
        else
          deadReqCodeGroupId.get.nes.map(ids => RestoreContent(Set.empty, ids.whole))
      )
    }
  }

  def setCustomTextField: Option[Gen[SetCustomTextField]] =
    for {
      id  <- liveReqId
      fid <- liveCustomFieldTextId
    } yield
      Gen.apply3(SetCustomTextField)(id, fid, customTextFieldText)

  def setGenericReqTitle: Option[Gen[SetGenericReqTitle]] =
    liveGenericReqId.map(id =>
      Gen.apply2(SetGenericReqTitle)(id, genericReqTitle))

  def setGenericReqType: Option[Gen[SetGenericReqType]] =
    for {
      gId <- liveGenericReqId
      gRT <- liveCustomReqTypeId
    } yield
      Gen.apply2(SetGenericReqType)(gId, gRT)

  def setUseCaseTitle: Option[Gen[SetUseCaseTitle]] =
    liveUseCaseId.map(id =>
      Gen.apply2(SetUseCaseTitle)(id, useCaseTitle))

  def shiftUseCaseStepLeft: Option[Gen[ShiftUseCaseStepLeft]] = {
    val ids = liveUseCaseIterator.flatMap(uc =>
      uc.stepsNA.tree.shiftLeftIterator((_, s) => s.id) ++
      uc.stepsE .tree.shiftLeftIterator((_, s) => s.id))
    Gen.tryGenChooseLazily(ids).map(_ map ShiftUseCaseStepLeft)
  }

  def shiftUseCaseStepRight: Option[Gen[ShiftUseCaseStepRight]] = {
    val ids = liveUseCaseIterator.flatMap(uc =>
      uc.stepsNA.tree.shiftRightIterator((_, s) => s.id) ++
      uc.stepsE .tree.shiftRightIterator((_, s) => s.id))
    Gen.tryGenChooseLazily(ids).map(_ map ShiftUseCaseStepRight)
  }

  def updateApplicableTag: Option[Gen[UpdateApplicableTag]] =
    liveApplicableTagId.map(gId =>
      for {
        id <- gId
        vs <- applicableTagGD.nonEmptyValues
      } yield {
        import ApplicableTagGD._
        UpdateApplicableTag(id, NonEmpty.force(emptyValues ++ vs.valuesIterator.map {
          case ValueForParents(v) => ValueForParents(v - id)
          case ValueForChildren(v) => ValueForChildren(v.filterNot(_ ==* id))
          case v => v
        }))
      }
    )

  def updateCustomImpField: Option[Gen[UpdateCustomImpField]] =
    for {
      id <- liveCustomFieldImpId
      vs <- customImpFieldGD.nonEmptyValues
    } yield
      Gen.apply2(UpdateCustomImpField)(id, vs)

  def updateCustomIssueType: Option[Gen[UpdateCustomIssueType]] =
    liveCustomIssueTypeId.map(id =>
      Gen.apply2(UpdateCustomIssueType)(id, customIssueTypeGD.nonEmptyValues))

  def updateCustomReqType: Option[Gen[UpdateCustomReqType]] =
    liveCustomReqTypeId.map(id =>
      Gen.apply2(UpdateCustomReqType)(id, customReqTypeGD.nonEmptyValues))

  def updateCustomTagField: Option[Gen[UpdateCustomTagField]] =
    for {
      id <- liveCustomFieldTagId
      vs <- customTagFieldGD.nonEmptyValues
    } yield
      Gen.apply2(UpdateCustomTagField)(id, vs)

  def updateCustomTextField: Option[Gen[UpdateCustomTextField]] =
    liveCustomFieldTextId.map(id =>
      Gen.apply2(UpdateCustomTextField)(id, customTextFieldGD.nonEmptyValues))

  def updateReqCodeGroup: Option[Gen[UpdateReqCodeGroup]] =
    liveReqCodeGroupId.map(id =>
      Gen.apply2(UpdateReqCodeGroup)(id, reqCodeGroupGD.nonEmptyValues))

  def updateTagGroup: Option[Gen[UpdateTagGroup]] =
    liveTagGroupId.map(gId =>
      for {
        id <- gId
        vs <- tagGroupGD.nonEmptyValues
      } yield {
        import TagGroupGD._
        UpdateTagGroup(id, NonEmpty.force(emptyValues ++ vs.valuesIterator.map {
          case ValueForParents(v) => ValueForParents(v - id)
          case ValueForChildren(v) => ValueForChildren(v.filterNot(_ ==* id))
          case v => v
        }))
      }
    )

  def updateUseCaseStep: Option[Gen[UpdateUseCaseStep]] =
    liveUseCase.map(genUseCase =>
      for {
        uc   <- genUseCase
        step <- Gen choose_! uc.stepIterator
        vs   <- useCaseStepGD(uc, step)
      } yield UpdateUseCaseStep(step.id, vs)
    )

  val possibleEventGens: NonEmptyVector[Option[Gen[Event]]] =
    valuesForAdt[Event, Option[Gen[Event]]] {
      case _: AddStaticField        => addStaticField
      case _: AddUseCaseStep        => addUseCaseStep
      case _: ApplyTemplate         => applyTemplate
      case _: CreateApplicableTag   => createApplicableTag
      case _: CreateCustomImpField  => createCustomImpField
      case _: CreateCustomIssueType => createCustomIssueType
      case _: CreateCustomReqType   => createCustomReqType
      case _: CreateCustomTagField  => createCustomTagField
      case _: CreateCustomTextField => createCustomTextField
      case _: CreateGenericReq      => createGenericReq
      case _: CreateReqCodeGroup    => createReqCodeGroup
      case _: CreateTagGroup        => createTagGroup
      case _: CreateUseCase         => createUseCase
      case _: DeleteCustomField     => deleteCustomField
      case _: DeleteCustomIssueType => deleteCustomIssueType
      case _: DeleteCustomReqType   => deleteCustomReqType
      case _: DeleteReqCodeGroups   => deleteReqCodeGroups
      case _: DeleteReqs            => deleteReqs
      case _: DeleteStaticField     => deleteStaticField
      case _: DeleteTag             => deleteTag
      case _: DeleteUseCaseStep     => deleteUseCaseStep
      case _: PatchImplicationSrc   => patchImplicationSrc
      case _: PatchImplicationTgt   => patchImplicationTgt
      case _: PatchReqCodes         => patchReqCodes
      case _: PatchReqTags          => patchReqTags
      case _: RepositionField       => repositionField
      case _: RestoreContent        => restoreContent
      case _: RestoreUseCaseStep    => restoreUseCaseStep
      case _: SetCustomTextField    => setCustomTextField
      case _: SetGenericReqTitle    => setGenericReqTitle
      case _: SetGenericReqType     => setGenericReqType
      case _: SetUseCaseTitle       => setUseCaseTitle
      case _: ShiftUseCaseStepLeft  => shiftUseCaseStepLeft
      case _: ShiftUseCaseStepRight => shiftUseCaseStepRight
      case _: UpdateApplicableTag   => updateApplicableTag
      case _: UpdateCustomImpField  => updateCustomImpField
      case _: UpdateCustomIssueType => updateCustomIssueType
      case _: UpdateCustomReqType   => updateCustomReqType
      case _: UpdateCustomTagField  => updateCustomTagField
      case _: UpdateCustomTextField => updateCustomTextField
      case _: UpdateReqCodeGroup    => updateReqCodeGroup
      case _: UpdateTagGroup        => updateTagGroup
      case _: UpdateUseCaseStep     => updateUseCaseStep
    }

  val eventGens: NonEmptyVector[Gen[Event]] =
    NonEmptyVector force possibleEventGens.iterator.filterDefined.toVector

  val eventGen: Gen[Event] =
    Gen chooseGenNE eventGens

//  def applyEventSG[S](observe: ObserveFn[S]): StateGen[S, (Event, Project)] =
//    StateGen.tailrec[S, (Event, Project)](s =>
//      eventGen.map { e =>
//        val r = ApplyEvent.untrusted.apply1(e)(p)
//        val s2 = observe(s, e, r)
//        r.bimap(_ => s2, p2 => (s2, (e, p2)))
//      }
//    )

  def applicableEventS[S](init: S)(observe: ObserveFn[S]): Gen[((S, Project), Event)] =
    BindRec[Gen].tailrecM((s: S) =>
      eventGen.map { e =>
        var r = ApplyEvent.untrusted.apply1(e)(p)
        r foreach { p2 => if (HashRec.changes(p, p2).isEmpty) r = -\/("No change") }
        val s2 = observe(s, e, r)
        r.bimap(_ => s2, p2 => ((s2, p2), e))
      }
    )(init)
}