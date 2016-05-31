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

  def tryGenChooseLiveDead[A](f: Live => TraversableOnce[A]): Live => Option[Gen[A]] =
    Live.memoLazy(l => Gen.tryGenChoose(f(l)))

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

  val tagId: Live => Option[Gen[TagId]] =
    tryGenChooseLiveDead(l => p.config.tags.valuesIterator.map(_.tag).filter(_.live :: l).map(_.id))

  val tagGroupId: Live => Option[Gen[TagGroupId]] =
    tryGenChooseLiveDead(l => p.config.tags.valuesIterator.map(_.tag).filterT[TagGroup].filter(_.live :: l).map(_.id))

  val applicableTagId: Live => Option[Gen[ApplicableTagId]] =
    tryGenChooseLiveDead(l => p.config.tags.valuesIterator.map(_.tag).filterT[ApplicableTag].filter(_.live :: l).map(_.id))

  lazy val existingApplicableTagId: Option[Gen[ApplicableTagId]] =
    Gen.tryGenChoose(p.config.tags.keysIterator.filterT[ApplicableTagId])

  def tagChildren: Gen[TagInTree.Children] =
    tagId(Live) match {
      case Some(g) => g.set.map(_.toVector)
      case None    => Gen pure Vector.empty
    }

  def tagParents: Gen[TagInTree.Parents] =
    tagId(Live) match {
      case Some(g) => g.option.mapBy(g)
      case None    => Gen pure Map.empty
    }

  val customReqTypeId: Live => Option[Gen[CustomReqTypeId]] =
    tryGenChooseLiveDead(l => cfg.reqTypes.custom.valuesIterator.filter(_.live :: l).map(_.id))

  lazy val applicableReqTypes: Gen[Field.ApplicableReqTypes] =
    RandomData.applicableReqTypes(cfg.reqTypes.custom.keySet)

  lazy val existingReqId: Option[Gen[ReqId]] =
    Gen.tryGenChoose(p.reqs.idIterator)

  lazy val liveReqIds: Vector[ReqId] =
    p.reqs.reqIterator.filter(_.live(cfg.reqTypes) :: Live).map(_.id).toVector

  lazy val liveReqId: Option[Gen[ReqId]] =
    Gen.tryGenChoose(liveReqIds)

  val genericReqId: Live => Option[Gen[GenericReqId]] =
    tryGenChooseLiveDead(l => p.reqs.genericReqs.valuesIterator.filter(_.live(cfg.reqTypes) :: l).map(_.id))

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

  val reqCodeGroupId: Live => Option[Gen[ReqCodeId]] =
    tryGenChooseLiveDead(l => p.reqCodes.groups.iterator.filter(_.live :: l).map(_.id).toVector)

  lazy val existingCustomIssueTypeId: Option[Gen[CustomIssueTypeId]] =
    Gen.tryGenChoose(cfg.customIssueTypes.keySet)

  val customIssueTypeId: Live => Option[Gen[CustomIssueTypeId]] =
    tryGenChooseLiveDead(l => cfg.customIssueTypes.valuesIterator.filter(_.live :: l).map(_.id))

  val customFieldId: Live => Option[Gen[CustomFieldId]] =
    tryGenChooseLiveDead(l => cfg.fields.customFields.valuesIterator.filter(_.live(cfg) :: l).map(_.id))

  val customFieldImpId: Live => Option[Gen[CustomField.Implication.Id]] =
    tryGenChooseLiveDead(l => cfg.customImpFields.filter(_.live(cfg) :: l).map(_.id))

  val customFieldTagId: Live => Option[Gen[CustomField.Tag.Id]] =
    tryGenChooseLiveDead(l => cfg.customTagFields.filter(_.live(cfg) :: l).map(_.id))

  val customFieldTextId: Live => Option[Gen[CustomField.Text.Id]] =
    tryGenChooseLiveDead(l => cfg.customTextFields.filter(_.live(cfg) :: l).map(_.id))

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
      case TagId     => tagId(Live)   map (_ map TagId    .apply)
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

  object createGenericReqGD extends GenericDataOptionGen(GenericReqGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Title    => genericReqTitle1                 map Title   .apply
      case ReqCodes => newReqCodeIdAndValue        .nes map ReqCodes.apply
      case Tags     => applicableTagId(Live) map (_.nes map Tags    .apply)
      case ImpSrcs  => liveReqId             map (_.nes map ImpSrcs .apply)
      case ImpTgts  => liveReqId             map (_.nes map ImpTgts .apply)
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

  object createUseCaseGD extends GenericDataOptionGen(UseCaseGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Title    => useCaseTitle1                    map Title   .apply
      case ReqCodes => newReqCodeIdAndValue        .nes map ReqCodes.apply
      case Tags     => applicableTagId(Live) map (_.nes map Tags    .apply)
      case ImpSrcs  => liveReqId             map (_.nes map ImpSrcs .apply)
      case ImpTgts  => liveReqId             map (_.nes map ImpTgts .apply)
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

  def genFieldStaticAdd: Option[Gen[FieldStaticAdd]] =
    Gen.tryGenChoose(staticFieldsToAdd).map(_ map FieldStaticAdd)

  def genUseCaseStepCreate: Option[Gen[UseCaseStepCreate]] =
    liveUseCase.map(genUC =>
      for {
        id    ← nextUseCaseStepId
        uc    ← genUC
        field ← Gen choose_! StaticField.useCaseStepTrees.whole.filter(_.useCaseStepTree.get(uc).nonEmpty)
        tree  = field.useCaseStepTree.get(uc)
        loc   ← tree.genParentLocation
      } yield UseCaseStepCreate(id, uc.id, field, loc)
    )

  def genProjectTemplateApply: Option[Gen[ProjectTemplateApply]] =
    if (p eq Project.empty)
      Some(RandomData.events.genProjectTemplateApply)
    else
      None

  def genApplicableTagCreate: Gen[ApplicableTagCreate] =
    Gen.apply2(ApplicableTagCreate)(nextApplicableTagId, applicableTagGD.allValues)

  def genFieldCustomImpCreate: Option[Gen[FieldCustomImpCreate]] =
    customImpFieldGD.allValues.map(vs =>
      Gen.apply2(FieldCustomImpCreate)(nextCustomFieldImplicationId, vs))

  def genCustomIssueTypeCreate: Gen[CustomIssueTypeCreate] =
    Gen.apply2(CustomIssueTypeCreate)(nextCustomIssueTypeId, customIssueTypeGD.allValues)

  def genCustomReqTypeCreate: Gen[CustomReqTypeCreate] =
    Gen.apply2(CustomReqTypeCreate)(nextCustomReqTypeId, customReqTypeGD.allValues)

  def genFieldCustomTagCreate: Option[Gen[FieldCustomTagCreate]] =
    customTagFieldGD.allValues.map(vs =>
      Gen.apply2(FieldCustomTagCreate)(nextCustomFieldTagId, vs))

  def genFieldCustomTextCreate: Gen[FieldCustomTextCreate] =
    Gen.apply2(FieldCustomTextCreate)(nextCustomFieldTextId, customTextFieldGD.allValues)

  def genGenericReqCreate: Option[Gen[GenericReqCreate]] =
    for {
      reqTypeId <- customReqTypeId(Live)
      //vs        <- createGenericReqGD.allPossibleValues
    } yield
      Gen.apply3(GenericReqCreate)(nextGenericReqId, reqTypeId, createGenericReqGD.values)

  def genReqCodeGroupCreate: Gen[ReqCodeGroupCreate] =
    Gen.apply2(ReqCodeGroupCreate)(nextReqCodeId, reqCodeGroupGD.allValues)

  def genTagGroupCreate: Gen[TagGroupCreate] =
    Gen.apply2(TagGroupCreate)(nextTagGroupId, tagGroupGD.allValues)

  def genUseCaseCreate: Gen[UseCaseCreate] =
    Gen.apply3(UseCaseCreate)(nextUseCaseId, nextUseCaseStepId, createUseCaseGD.values)

  def genFieldCustomDelete: Option[Gen[FieldCustomDelete]] =
    customFieldId(Live).map(_ map FieldCustomDelete)

  def genFieldCustomRestore: Option[Gen[FieldCustomRestore]] =
    customFieldId(Dead).map(_ map FieldCustomRestore)

  def genCustomIssueTypeDelete: Option[Gen[CustomIssueTypeDelete]] =
    customIssueTypeId(Live).map(_ map CustomIssueTypeDelete)

  def genCustomReqTypeDelete: Option[Gen[CustomReqTypeDelete]] =
    customReqTypeId(Live).map(_ map CustomReqTypeDelete)

  def genReqCodeGroupsDelete: Option[Gen[ReqCodeGroupsDelete]] =
    reqCodeGroupId(Live).map(_.nes map ReqCodeGroupsDelete)

  def genCustomIssueTypeRestore: Option[Gen[CustomIssueTypeRestore]] =
    customIssueTypeId(Dead).map(_ map CustomIssueTypeRestore)

  def genCustomReqTypeRestore: Option[Gen[CustomReqTypeRestore]] =
    customReqTypeId(Dead).map(_ map CustomReqTypeRestore)

  def genReqsDelete: Option[Gen[ReqsDelete]] =
    liveReqId.map(reqId =>
      Gen.apply3(ReqsDelete)(reqId.nes, reqCodeGroupId(Live).setE, deletionReason))

  def genFieldStaticRemove: Option[Gen[FieldStaticRemove]] =
    Gen.tryGenChoose(staticFieldsToDel).map(_ map FieldStaticRemove)

  def genTagDelete: Option[Gen[TagDelete]] =
    tagId(Live).map(_ map TagDelete)

  def genTagRestore: Option[Gen[TagRestore]] =
    tagId(Dead).map(_ map TagRestore)

  def genUseCaseStepDelete: Option[Gen[UseCaseStepDelete]] = {
    val ids = p.useCaseStepsDeletable.map(_.id)
    Gen.tryGenChooseLazily(ids).map(_ map UseCaseStepDelete)
  }

  def genUseCaseStepRestore: Option[Gen[UseCaseStepRestore]] = {
    val ids = p.useCaseStepsRestorable.map(_.id)
    Gen.tryGenChooseLazily(ids).map(_ map UseCaseStepRestore)
  }

  def genReqImplicationsPatch: Option[Gen[ReqImplicationsPatch]] =
    if (liveReqIds.length < 2)
      None
    else
      liveReqId.map { gReqId =>
        RandomData.dir.flatMap { dir =>
          gReqId.flatMap { id =>
            Gen.choose_!(liveReqIds.filter(_ !=* id)).set1.map { ids =>
              val sd = SetDiff.xor(p.implications(dir)(id), ids)
              ReqImplicationsPatch(id, dir, NonEmpty force sd)
            }
          }
        }
      }

  def genReqCodesPatch: Option[Gen[ReqCodesPatch]] =
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
        ReqCodesPatch(reqId, remove, restore, Multimap(add.toMap))
    )

  def genReqTagsPatch: Option[Gen[ReqTagsPatch]] =
    for {
      gId  <- liveReqId
      gTag <- existingApplicableTagId
    } yield for {
      id   <- gId
      tags <- gTag.nes(1 to 5, implicitly)
    } yield {
      val sd = SetDiff.xor(p.reqTags(id), tags.whole)
      ReqTagsPatch(id, NonEmpty force sd)
    }

  def genFieldReposition: Gen[FieldReposition] =
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
      } yield FieldReposition(id, pos)
    }

  def genContentRestore: Option[Gen[ContentRestore]] = {
    val restorableReqIds = Gen.tryGenChoose[ReqId](
      p.reqs.reqIterator.filter {
        case g: GenericReq => (g.liveExplicitly :: Dead) && (g.copy(liveExplicitly = Live).live(cfg.reqTypes) :: Live)
        case u: UseCase    => (u.liveExplicitly :: Dead) && (u.copy(liveExplicitly = Live).live(cfg.reqTypes) :: Live)
      }.map(_.id).toVector)
    if (restorableReqIds.isEmpty && reqCodeGroupId(Dead).isEmpty)
      None
    else Some {
      val idSet = restorableReqIds.setE
      val codeSet = reqCodeGroupId(Dead).setE
      Gen.apply2(ContentRestore)(idSet, codeSet).flatMap(cmd =>
        if (cmd.reqs.nonEmpty || cmd.reqCodeGroups.nonEmpty)
          Gen pure cmd
        else if (restorableReqIds.isDefined)
          restorableReqIds.get.nes.map(ids => ContentRestore(ids.whole, Set.empty))
        else
          reqCodeGroupId(Dead).get.nes.map(ids => ContentRestore(Set.empty, ids.whole))
      )
    }
  }

  def genReqFieldCustomTextSet: Option[Gen[ReqFieldCustomTextSet]] =
    for {
      id  <- liveReqId
      fid <- customFieldTextId(Live)
    } yield
      Gen.apply3(ReqFieldCustomTextSet)(id, fid, customTextFieldText)

  def genGenericReqTitleSet: Option[Gen[GenericReqTitleSet]] =
    genericReqId(Live).map(id =>
      Gen.apply2(GenericReqTitleSet)(id, genericReqTitle))

  def genGenericReqTypeSet: Option[Gen[GenericReqTypeSet]] =
    for {
      gId <- genericReqId(Live)
      gRT <- customReqTypeId(Live)
    } yield
      Gen.apply2(GenericReqTypeSet)(gId, gRT)

  def genUseCaseTitleSet: Option[Gen[UseCaseTitleSet]] =
    liveUseCaseId.map(id =>
      Gen.apply2(UseCaseTitleSet)(id, useCaseTitle))

  def genUseCaseStepShiftLeft: Option[Gen[UseCaseStepShiftLeft]] = {
    val ids = liveUseCaseIterator.flatMap(uc =>
      uc.stepsNA.tree.shiftLeftIterator((_, s) => s.id) ++
      uc.stepsE .tree.shiftLeftIterator((_, s) => s.id))
    Gen.tryGenChooseLazily(ids).map(_ map UseCaseStepShiftLeft)
  }

  def genUseCaseStepShiftRight: Option[Gen[UseCaseStepShiftRight]] = {
    val ids = liveUseCaseIterator.flatMap(uc =>
      uc.stepsNA.tree.shiftRightIterator((_, s) => s.id) ++
      uc.stepsE .tree.shiftRightIterator((_, s) => s.id))
    Gen.tryGenChooseLazily(ids).map(_ map UseCaseStepShiftRight)
  }

  def genApplicableTagUpdate: Option[Gen[ApplicableTagUpdate]] =
    applicableTagId(Live).map(gId =>
      for {
        id <- gId
        vs <- applicableTagGD.nonEmptyValues
      } yield {
        import ApplicableTagGD._
        ApplicableTagUpdate(id, NonEmpty.force(emptyValues ++ vs.valuesIterator.map {
          case ValueForParents(v) => ValueForParents(v - id)
          case ValueForChildren(v) => ValueForChildren(v.filterNot(_ ==* id))
          case v => v
        }))
      }
    )

  def genFieldCustomImpUpdate: Option[Gen[FieldCustomImpUpdate]] =
    for {
      id <- customFieldImpId(Live)
      vs <- customImpFieldGD.nonEmptyValues
    } yield
      Gen.apply2(FieldCustomImpUpdate)(id, vs)

  def genCustomIssueTypeUpdate: Option[Gen[CustomIssueTypeUpdate]] =
    customIssueTypeId(Live).map(id =>
      Gen.apply2(CustomIssueTypeUpdate)(id, customIssueTypeGD.nonEmptyValues))

  def genCustomReqTypeUpdate: Option[Gen[CustomReqTypeUpdate]] =
    customReqTypeId(Live).map(id =>
      Gen.apply2(CustomReqTypeUpdate)(id, customReqTypeGD.nonEmptyValues))

  def genFieldCustomTagUpdate: Option[Gen[FieldCustomTagUpdate]] =
    for {
      id <- customFieldTagId(Live)
      vs <- customTagFieldGD.nonEmptyValues
    } yield
      Gen.apply2(FieldCustomTagUpdate)(id, vs)

  def genFieldCustomTextUpdate: Option[Gen[FieldCustomTextUpdate]] =
    customFieldTextId(Live) .map(id =>
      Gen.apply2(FieldCustomTextUpdate)(id, customTextFieldGD.nonEmptyValues))

  def genReqCodeGroupUpdate: Option[Gen[ReqCodeGroupUpdate]] =
    reqCodeGroupId(Live).map(id =>
      Gen.apply2(ReqCodeGroupUpdate)(id, reqCodeGroupGD.nonEmptyValues))

  def genTagGroupUpdate: Option[Gen[TagGroupUpdate]] =
    tagGroupId(Live).map(gId =>
      for {
        id <- gId
        vs <- tagGroupGD.nonEmptyValues
      } yield {
        import TagGroupGD._
        TagGroupUpdate(id, NonEmpty.force(emptyValues ++ vs.valuesIterator.map {
          case ValueForParents(v) => ValueForParents(v - id)
          case ValueForChildren(v) => ValueForChildren(v.filterNot(_ ==* id))
          case v => v
        }))
      }
    )

  def genUseCaseStepUpdate: Option[Gen[UseCaseStepUpdate]] =
    liveUseCase.map(genUseCase =>
      for {
        uc   <- genUseCase
        step <- Gen choose_! uc.stepIterator
        vs   <- useCaseStepGD(uc, step)
      } yield UseCaseStepUpdate(step.id, vs)
    )

  def genProjectNameSet: Gen[ProjectNameSet] =
    RandomData.projectName.map(Some(_).filter(_ !=* p.name)).optionGet map ProjectNameSet

  val possibleEventGens: NonEmptyVector[Option[Gen[Event]]] =
    valuesForAdt[Event, Option[Gen[Event]]] {
      case _: ApplicableTagCreate    => genApplicableTagCreate
      case _: ApplicableTagUpdate    => genApplicableTagUpdate
      case _: ContentRestore         => genContentRestore
      case _: CustomIssueTypeCreate  => genCustomIssueTypeCreate
      case _: CustomIssueTypeDelete  => genCustomIssueTypeDelete
      case _: CustomIssueTypeRestore => genCustomIssueTypeRestore
      case _: CustomIssueTypeUpdate  => genCustomIssueTypeUpdate
      case _: CustomReqTypeCreate    => genCustomReqTypeCreate
      case _: CustomReqTypeDelete    => genCustomReqTypeDelete
      case _: CustomReqTypeRestore   => genCustomReqTypeRestore
      case _: CustomReqTypeUpdate    => genCustomReqTypeUpdate
      case _: FieldCustomDelete      => genFieldCustomDelete
      case _: FieldCustomImpCreate   => genFieldCustomImpCreate
      case _: FieldCustomImpUpdate   => genFieldCustomImpUpdate
      case _: FieldCustomRestore     => genFieldCustomRestore
      case _: FieldCustomTagCreate   => genFieldCustomTagCreate
      case _: FieldCustomTagUpdate   => genFieldCustomTagUpdate
      case _: FieldCustomTextCreate  => genFieldCustomTextCreate
      case _: FieldCustomTextUpdate  => genFieldCustomTextUpdate
      case _: FieldReposition        => genFieldReposition
      case _: FieldStaticAdd         => genFieldStaticAdd
      case _: FieldStaticRemove      => genFieldStaticRemove
      case _: GenericReqCreate       => genGenericReqCreate
      case _: GenericReqTitleSet     => genGenericReqTitleSet
      case _: GenericReqTypeSet      => genGenericReqTypeSet
      case _: ProjectNameSet         => genProjectNameSet
      case _: ProjectTemplateApply   => genProjectTemplateApply
      case _: ReqCodeGroupCreate     => genReqCodeGroupCreate
      case _: ReqCodeGroupsDelete    => genReqCodeGroupsDelete
      case _: ReqCodeGroupUpdate     => genReqCodeGroupUpdate
      case _: ReqCodesPatch          => genReqCodesPatch
      case _: ReqFieldCustomTextSet  => genReqFieldCustomTextSet
      case _: ReqImplicationsPatch   => genReqImplicationsPatch
      case _: ReqsDelete             => genReqsDelete
      case _: ReqTagsPatch           => genReqTagsPatch
      case _: TagDelete              => genTagDelete
      case _: TagGroupCreate         => genTagGroupCreate
      case _: TagGroupUpdate         => genTagGroupUpdate
      case _: TagRestore             => genTagRestore
      case _: UseCaseCreate          => genUseCaseCreate
      case _: UseCaseStepCreate      => genUseCaseStepCreate
      case _: UseCaseStepDelete      => genUseCaseStepDelete
      case _: UseCaseStepRestore     => genUseCaseStepRestore
      case _: UseCaseStepShiftLeft   => genUseCaseStepShiftLeft
      case _: UseCaseStepShiftRight  => genUseCaseStepShiftRight
      case _: UseCaseStepUpdate      => genUseCaseStepUpdate
      case _: UseCaseTitleSet        => genUseCaseTitleSet
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
