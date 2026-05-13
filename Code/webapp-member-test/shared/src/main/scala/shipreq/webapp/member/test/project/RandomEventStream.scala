package shipreq.webapp.member.test.project

import cats.Monad
import cats.instances.vector._
import cats.syntax.traverse._
import japgolly.microlibs.adt_macros.AdtMacros._
import japgolly.microlibs.nonempty.NonEmpty
import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Instant
import nyaya.gen._
import nyaya.prop.LogicPropExt
import shipreq.base.test.BaseUtilGen._
import shipreq.base.test.Incrementor
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.data.{ProjectCreator, ProjectRole, UserId}
import shipreq.webapp.base.test.RandomBaseData
import shipreq.webapp.base.test.RandomBaseData.unicodeString1
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview.SavedView
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event.RetiredGenericData._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.text.Text
import shipreq.webapp.member.test.WebappBaseGen._
import shipreq.webapp.member.test.WebappTestUtil
import shipreq.webapp.member.test.project.ApplicableEventGen.ObserveFn
import shipreq.webapp.member.test.project.DataTestExt._
import shipreq.webapp.member.test.project.RandomData
import shipreq.webapp.member.test.project.RandomData.{TextGen, TextGenExt, customReqTypeName, desc, exclusivity, fieldName, fieldRefKey, filter, filterDead, genColour, hashRefKey, implicationRequired, mandatory, projectRole, reqCode, reqTypeMnemonic, tagGroupName}
import shipreq.webapp.member.test.project.RandomEventStream.{ProjectDepGen, State}
import shipreq.webapp.server.logic.util.Obfuscators

final case class RandomEventStreamConfig(retiredEvents: Boolean,
                                         reqCodeEvents: Boolean,
                                         userIds      : Option[NonEmptySet[UserId.Public]],
                                         emptyProject : Project,
                                        ) {

  def activeOnly: RandomEventStreamConfig =
    copy(retiredEvents = false)

  def withUserIds(ids: NonEmptySet[UserId]): RandomEventStreamConfig =
    withPublicUserIds(ids.map(Obfuscators.userId.obfuscate))

  def withPublicUserIds(ids: NonEmptySet[UserId.Public]): RandomEventStreamConfig =
    copy(userIds = Some(ids))

  def withEmptyProject(p: Project): RandomEventStreamConfig =
    copy(emptyProject = p)

  def withCreator(p: ProjectCreator): RandomEventStreamConfig =
    withEmptyProject(Project.init(p))
}

object RandomEventStreamConfig {
  val default = apply(
    retiredEvents = true,
    reqCodeEvents = true,
    userIds       = None,
    emptyProject  = WebappTestUtil.emptyProject1,
  )
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

/** Generates a random event stream that can be successfully applied.
  *
  * This differs from the events that [[RandomData]] can generate which are only valid in isolation and often don't
  * make sense as a consecutive stream.
  */
object RandomEventStream extends RandomEventStreamDsl(RandomEventStreamConfig.default.emptyProject, ApplicableEventGen(_, _)) {

  type State = Project

  type ProjectDepGen[A] = StateGen[State, A]

  def liftGE(eventGen: Gen[Event]): ProjectDepGen[VerifiedEvent] =
    liftPGE(_ => eventGen)

  def liftPGE(eventGen: State => Gen[Event], maxAttempts: Int = 60): ProjectDepGen[VerifiedEvent] =
    StateGen(p1 =>
      eventGen(p1).map(e =>
        ApplyEvent.untrusted.partialApplyUnverified(e)(p1) match {
          case \/-(p2) => Some {
            val ve = VerifiedEvent(p1.history.nextOrd, e, Instant.now())
            val p3 = Project.history.modify(_ + ve)(p2)
            (p3, ve)
          }
          case -\/(_)  => None
        }
      ).optionGetLimit(maxAttempts)
    )

  private[project] def keepProject[A](g: ProjectDepGen[A]): ProjectDepGen[(A, Project)] =
    StateGen(s => g.run(s).map(x => (x._1, (x._2, x._1))))

  val InitialEventCount = 2

  private[project] def initialEventGens(emptyState: State): Vector[ProjectDepGen[VerifiedEvent]] =
    Vector(
      liftGE(RandomData.events.genProjectTemplateApply),
      liftPGE(ApplicableEventGen(emptyState, _).genProjectNameSet),
    )

  def initialEvents(emptyState: State): Gen[(State, Vector[VerifiedEvent])] =
    initialEventGens(emptyState).sequence.run(emptyState)

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
//    val SSS = cats.StateT.stateTMonadState[((S, Project), VerifiedEvents), Gen]
//    import SSS.monadSyntax._
//    // TO DO Speed up replicateM_ (?)
//    StateGen(spv => ss.gen.flatMap(n => gen.replicateM_(n)(spv)))
//  }
//
//  def genEventStreamS[S](s0: S, p0: Project = x)(observe: ObserveFn[S])(implicit ss: SizeSpec): Gen[(S, VerifiedEvents)] =
//    eventStreamS(addVerifiedEventS(applicableEventS(observe)))(ss)((s0, p0), Vector.empty)
//      .map { case (((s, _), vs), ()) => (s, vs) }
//
//  def withEventStats(p: Project = x)(implicit ss: SizeSpec): Gen[(EventStats, VerifiedEvents)] =
//    genEventStreamS(EventStats.empty, p)(EventStats.observeFn)

  def withConfig(mod: RandomEventStreamConfig => RandomEventStreamConfig): RandomEventStreamDsl =
    withConfig(mod(RandomEventStreamConfig.default))

  def withConfig(cfg: RandomEventStreamConfig): RandomEventStreamDsl =
    new RandomEventStreamDsl(cfg.emptyProject, ApplicableEventGen(_, _, cfg))

  val activeOnly: RandomEventStreamDsl =
    withConfig(_.copy(retiredEvents = false))

  val projectRoleOptions = ProjectRole.values.iterator.map(Option(_)).toSet + None
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

sealed class RandomEventStreamDsl(emptyState: State, applicableEventGen: (State, State) => ApplicableEventGen) {
  import RandomEventStream.{initialEvents, initialEventGens, keepProject, liftPGE}

  val verifiedEvent: ProjectDepGen[VerifiedEvent] =
    StateGen(applicableEventGen(emptyState, _).verifiedEvent)

  def verifiedEvents(implicit ss: SizeSpec): ProjectDepGen[Vector[VerifiedEvent]] =
    StateGen(state =>
      ss.gen.flatMap(size =>
        Vector.fill(size)(verifiedEvent).sequence.run(state)))

  def verifiedEventMatching(accept: Event => Boolean, maxAttempts: Int = 100): ProjectDepGen[VerifiedEvent] =
    liftPGE(ApplicableEventGen(emptyState, _).eventGen.map(e => Option.when(accept(e))(e)).optionGetLimit(maxAttempts))

  def verifiedEventOfTypes(types: NonEmptySet[EventName]): ProjectDepGen[VerifiedEvent] =
    liftPGE(ApplicableEventGen(emptyState, _).eventGenOfTypes(types))

  def entireEventStream(implicit ss: SizeSpec): Gen[(State, Vector[VerifiedEvent], Vector[VerifiedEvent])] =
    for {
      (s1, e1) <- initialEvents(emptyState)
      (s2, e2) <- verifiedEvents(ss).run(s1)
    } yield (s2, e1, e2)

  def justEntireEventStream(implicit ss: SizeSpec): Gen[Vector[VerifiedEvent]] =
    for {
      (_, e1, e2) <- entireEventStream(ss)
    } yield e1 ++ e2

  def eventStreamWithProjects(implicit ss: SizeSpec): Gen[Vector[(VerifiedEvent, Project)]] = {
    StateGen { (state: State) =>
      ss.gen.flatMap { size =>
        def steps() = initialEventGens(emptyState).iterator.map(keepProject) ++ Iterator.fill(size)(keepProject(verifiedEvent))
        steps().toVector.sequence.run(state)
      }
    }.runA(emptyState)
  }

  lazy val sampleEventStreamWithProjects: Vector[(VerifiedEvent, Project)] =
    eventStreamWithProjects(100).sample()
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ApplicableEventGen {
  def apply(emptyState: State, curState: State, config: RandomEventStreamConfig = RandomEventStreamConfig.default): ApplicableEventGen =
    new ApplicableEventGen(emptyState, curState, config)

  type ObserveFn[S] = (S, Event, ApplyEvent.Result) => S
}

final class ApplicableEventGen(emptyState: State, curState: State, config: RandomEventStreamConfig) {
  val p = curState

  private implicit val gss: SizeSpec = 0 to 3

  // If the starting state isn't valid, event application will never succeed and thus, loop forever
  if (p ne emptyState)
    DataProp.project.allIncludingConfig assert p

  private val cfg = p.config

  private implicit def autoGenToOptionGen[A](g: Gen[A]): Option[Gen[A]] = Some(g)

  def tryGenChooseLiveDead[A](f: Live => IterableOnce[A]): Live => Option[Gen[A]] =
    Live.memoLazy(l => Gen.tryGenChoose(f(l)))

  val (staticFieldsToDel, staticFieldsToAdd) =
    StaticField.optional.whole.partition(cfg.fields.staticFieldSet.contains)

  val nextReqId: Gen[Int] =
    Incrementor.int gen p.idCeilings.req

  val nextGenericReqId: Gen[GenericReqId] =
    nextReqId map GenericReqId

  val nextReqCodeIdA: Gen[ApReqCodeId] =
    Incrementor.int gen p.idCeilings.reqCode map ApReqCodeId.apply

  val nextReqCodeIdG: Gen[ReqCodeGroupId] =
    Incrementor.int gen p.idCeilings.reqCode map ReqCodeGroupId

  val nextCustomIssueTypeId: Gen[CustomIssueTypeId] =
    Incrementor.int gen p.idCeilings.customIssueType map CustomIssueTypeId

  val nextCustomReqTypeId: Gen[CustomReqTypeId] =
    Incrementor.int gen p.idCeilings.customReqType map CustomReqTypeId

  val nextCustomField: Gen[Int] =
    Incrementor.int gen p.idCeilings.customField

  val nextCustomFieldImplicationId: Gen[CustomField.Implication.Id] =
    nextCustomField map CustomField.Implication.Id

  val nextCustomFieldTagId: Gen[CustomField.Tag.Id] =
    nextCustomField map CustomField.Tag.Id

  val nextCustomFieldTextId: Gen[CustomField.Text.Id] =
    nextCustomField map CustomField.Text.Id

  val nextTagId: Gen[Int] =
    Incrementor.int gen p.idCeilings.tag

  val nextApplicableTagId: Gen[ApplicableTagId] =
    nextTagId map ApplicableTagId

  val nextTagGroupId: Gen[TagGroupId] =
    nextTagId map TagGroupId

  val nextUseCaseId: Gen[UseCaseId] =
    nextReqId map UseCaseId

  val nextUseCaseStepId: Gen[UseCaseStepId] =
    Incrementor.int gen p.idCeilings.useCaseStep map UseCaseStepId

  val nextSavedViewId: Gen[SavedView.Id] =
    Incrementor.int gen p.idCeilings.savedView map SavedView.Id

  val tagId: Live => Option[Gen[TagId]] =
    tryGenChooseLiveDead(l => p.config.tags.tree.valuesIterator.map(_.tag).filter(_.live is l).map(_.id))

  val tagGroupId: Live => Option[Gen[TagGroupId]] =
    tryGenChooseLiveDead(l => p.config.tags.tree.valuesIterator.map(_.tag).filterSubType[TagGroup].filter(_.live is l).map(_.id))

  val applicableTagId: Live => Option[Gen[ApplicableTagId]] =
    tryGenChooseLiveDead(l => p.config.tags.tree.valuesIterator.map(_.tag).filterSubType[ApplicableTag].filter(_.live is l).map(_.id))

  lazy val applicableTagIdSet =
    p.config.tags.tree.keysIterator.filterSubType[ApplicableTagId].toSet

  lazy val existingApplicableTagId: Option[Gen[ApplicableTagId]] =
    Gen.tryGenChoose(applicableTagIdSet)

  lazy val existingReqType: Gen[ReqType] =
    Gen.chooseNE(p.config.reqTypes.all)

  lazy val existingReqTypeId: Gen[ReqTypeId] =
    existingReqType.map(_.reqTypeId)

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
    tryGenChooseLiveDead(l => cfg.reqTypes.custom.valuesIterator.filter(_.live is l).map(_.id))

  lazy val applicableReqTypes: Gen[ApplicableReqTypes] =
    RandomData.applicableReqTypes(cfg.reqTypes.custom.keySet)

  lazy val fieldReqTypeRules_ : Gen[FieldReqTypeRules[Impossible]] =
    RandomData.fieldReqTypeRules(existingReqTypeId, None)

  lazy val fieldReqTypeRulesTag: Gen[FieldReqTypeRules[ApplicableTagId]] =
    RandomData.fieldReqTypeRules(existingReqTypeId, existingApplicableTagId)

  lazy val derivativeTags: Gen[DerivativeTags] =
    RandomData.derivativeTags(applicableTagIdSet)

  lazy val existingReqId: Option[Gen[ReqId]] =
    Gen.tryGenChoose(p.content.reqs.idIterator())

  lazy val liveReqIds: Vector[ReqId] =
    p.content.reqs.reqIterator().filter(_.live(cfg.reqTypes) is Live).map(_.id).toVector

  lazy val liveReqId: Option[Gen[ReqId]] =
    Gen.tryGenChoose(liveReqIds)

  val genericReqId: Live => Option[Gen[GenericReqId]] =
    tryGenChooseLiveDead(l => p.content.reqs.genericReqs.imap.valuesIterator.filter(_.live(cfg.reqTypes) is l).map(_.id))

  def liveUseCaseIterator: Iterator[UseCase] =
    p.content.reqs.useCases.imap.valuesIterator.filter(_.liveUC is Live)

  lazy val liveUseCase: Option[Gen[UseCase]] =
    Gen.tryGenChoose(liveUseCaseIterator)

  lazy val liveUseCaseId: Option[Gen[UseCaseId]] =
    liveUseCase.map(_.map(_.id))

  lazy val existingUseCaseStepId: Option[Gen[UseCaseStepId]] =
    Gen.tryGenChoose(p.content.reqs.useCases.stepIterator.map(_.id))

  lazy val existingReqCodeId: Option[Gen[ReqCodeId]] =
    Gen.tryGenChoose(p.content.reqCodes.idSeq)

  val codeGroupId: Live => Option[Gen[ReqCodeGroupId]] =
    tryGenChooseLiveDead(l => p.content.reqCodes.groups.iterator.filter(_.live is l).map(_.id).toVector)

  lazy val existingCustomIssueTypeId: Option[Gen[CustomIssueTypeId]] =
    Gen.tryGenChoose(cfg.customIssueTypes.keySet)

  val customIssueTypeId: Live => Option[Gen[CustomIssueTypeId]] =
    tryGenChooseLiveDead(l => cfg.customIssueTypes.valuesIterator.filter(_.live is l).map(_.id))

  val customFieldId: Live => Option[Gen[CustomFieldId]] =
    tryGenChooseLiveDead(l => cfg.fields.customFields.valuesIterator.filter(_.live(cfg) is l).map(_.id))

  val customFieldImpId: Live => Option[Gen[CustomField.Implication.Id]] =
    tryGenChooseLiveDead(l => cfg.fields.customImpFields.filter(_.live(cfg) is l).map(_.id))

  val customFieldTagId: Live => Option[Gen[CustomField.Tag.Id]] =
    tryGenChooseLiveDead(l => cfg.fields.customTagFields.filter(_.live(cfg) is l).map(_.id))

  val customFieldTextId: Live => Option[Gen[CustomField.Text.Id]] =
    tryGenChooseLiveDead(l => cfg.fields.customTextFields.filter(_.live(cfg) is l).map(_.id))

  lazy val customTextFieldTextAtom: Gen[Text.CustomTextField.Atom] =
    TextGen.customTextFieldAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingCustomIssueTypeId, existingApplicableTagId)

  def customTextFieldText: Gen[Text.CustomTextField.OptionalText] =
    customTextFieldTextAtom.text

  def customTextFieldText1: Gen[Text.CustomTextField.NonEmptyText] =
    customTextFieldTextAtom.text1(Text.CustomTextField)

  lazy val newReqCodeIdAndValue: Gen[ApReqCodeId.AndValue] =
    Gen.apply2(ApReqCodeId.AndValue)(nextReqCodeIdA, reqCode.value)

  def codeGroupTitle: Gen[Text.CodeGroupTitle.OptionalText] =
    TextGen.codeGroupTitleAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingCustomIssueTypeId).text

  private lazy val genericReqTitleAtom: Gen[Text.GenericReqTitle.Atom] =
    TextGen.genericReqTitleAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingCustomIssueTypeId, existingApplicableTagId)

  def genericReqTitle: Gen[Text.GenericReqTitle.OptionalText] =
    genericReqTitleAtom.text

  def genericReqTitle1: Gen[Text.GenericReqTitle.NonEmptyText] =
    genericReqTitleAtom.text1(Text.GenericReqTitle)

  def deletionReason: Gen[Text.DeletionReason.OptionalText] =
    TextGen.deletionReasonAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingApplicableTagId).text

  lazy val useCaseTitleAtom: Gen[Text.UseCaseTitle.Atom] =
    TextGen.useCaseTitleAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingCustomIssueTypeId, existingApplicableTagId)

  def useCaseTitle: Gen[Text.UseCaseTitle.OptionalText] =
    useCaseTitleAtom.text

  def useCaseTitle1: Gen[Text.UseCaseTitle.NonEmptyText] =
    useCaseTitleAtom.text1(Text.UseCaseTitle)

  def useCaseStepTitle: Gen[Text.UseCaseStep.OptionalText] =
    TextGen.useCaseStepAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingCustomIssueTypeId, existingApplicableTagId).text

  lazy val nonEmptyCustomTextMap: Option[Gen[Event.NonEmptyCustomTextMap]] =
    customFieldTextId(Live).map(_.mapTo(customTextFieldText1)(1 to 3).map(NonEmpty.force))

  object customIssueTypeGD extends GenericDataGen(CustomIssueTypeGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Key  => hashRefKey map Key .apply
      case Desc => desc       map Desc.apply
    }
  }

  object customReqTypeGDv1 extends GenericDataGen(CustomReqTypeGDv1) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Name        => customReqTypeName   map Name       .apply
      case Implication => implicationRequired map Implication.apply
      case gd.Mnemonic => reqTypeMnemonic     map gd.Mnemonic.apply
    }
  }

  object customReqTypeGD extends GenericDataGen(CustomReqTypeGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Name        => customReqTypeName   map Name       .apply
      case Description => desc                map Description.apply
      case Implication => implicationRequired map Implication.apply
      case gd.Mnemonic => reqTypeMnemonic     map gd.Mnemonic.apply
    }
  }

  object customTextFieldGDv1 extends GenericDataGen(CustomTextFieldGDv1) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Name               => fieldName          map Name     .apply
      case Key                => fieldRefKey        map Key      .apply
      case Mandatory          => mandatory          map Mandatory.apply
      case ApplicableReqTypes => applicableReqTypes map ApplicableReqTypes .apply
    }
  }

  object customTagFieldGDv1 extends GenericDataOptionGen(CustomTagFieldGDv1) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case TagId              => tagId(Live)   map (_ map TagId    .apply)
      case Mandatory          => mandatory            map Mandatory.apply
      case ApplicableReqTypes => applicableReqTypes   map ApplicableReqTypes .apply
    }
  }

  object customImpFieldGDv1 extends GenericDataOptionGen(CustomImpFieldGDv1) {
    import gd._

    private def reqTypesUsedInFields: Set[ReqTypeId] =
      cfg.fields.customImpFields.map(_.reqTypeId).toSet

    private def liveReqTypes: Iterator[ReqTypeId] =
      StaticReqType.values.iterator.map(_.reqTypeId) ++
      cfg.reqTypes.custom.valuesIterator.filter(_.live is Live).map(_.id)

    private def reqTypeId: Option[Gen[ReqTypeId]] =
      Gen.tryGenChoose(liveReqTypes.filterNot(reqTypesUsedInFields.contains))

    override def valueFor(a: Attr) = a match {
      case ReqTypeId          => reqTypeId          map (_ map ReqTypeId.apply)
      case Mandatory          => mandatory                 map Mandatory.apply
      case ApplicableReqTypes => applicableReqTypes        map ApplicableReqTypes .apply
    }
  }

  object customTextFieldGD extends GenericDataGen(CustomTextFieldGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Name              => fieldName          map Name             .apply
      case FieldReqTypeRules => fieldReqTypeRules_ map FieldReqTypeRules.apply
    }
  }

  object customTagFieldGD extends GenericDataOptionGen(CustomTagFieldGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case FieldReqTypeRules => fieldReqTypeRulesTag map FieldReqTypeRules.apply
      case DerivativeTags    => derivativeTags       map DerivativeTags   .apply
    }
  }

  object customImpFieldGD extends GenericDataOptionGen(CustomImpFieldGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case FieldReqTypeRules => fieldReqTypeRules_ map FieldReqTypeRules.apply
    }
  }

  object createGenericReqGD extends GenericDataOptionGen(GenericReqGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Codes      => newReqCodeIdAndValue        .nes map Codes     .apply
      case CustomText => nonEmptyCustomTextMap map (_     map CustomText.apply)
      case ImpSrcs    => liveReqId             map (_.nes map ImpSrcs   .apply)
      case ImpTgts    => liveReqId             map (_.nes map ImpTgts   .apply)
      case Tags       => applicableTagId(Live) map (_.nes map Tags      .apply)
      case Title      => genericReqTitle1                 map Title     .apply
    }
  }

  object codeGroupGD extends GenericDataGen(CodeGroupGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Code  => reqCode.value  map Code .apply
      case Title => codeGroupTitle map Title.apply
    }
  }

  object applicableTagGDv1 extends GenericDataGen(ApplicableTagGDv1) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Name     => unicodeString1 map Name    .apply
      case Desc     => desc           map Desc    .apply
      case Key      => hashRefKey     map Key     .apply
      case Children => tagChildren    map Children.apply
      case Parents  => tagParents     map Parents .apply
    }
  }

  object applicableTagGD extends GenericDataGen(ApplicableTagGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case ApplicableReqTypes => applicableReqTypes map ApplicableReqTypes.apply
      case Colour             => genColour.option   map Colour  .apply
      case Desc               => desc               map Desc    .apply
      case Key                => hashRefKey         map Key     .apply
      case Children           => tagChildren        map Children.apply
      case Parents            => tagParents         map Parents .apply
    }
  }

  object tagGroupGD extends GenericDataGen(TagGroupGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Name        => tagGroupName map Name       .apply
      case Desc        => desc         map Desc       .apply
      case Exclusivity => exclusivity  map Exclusivity.apply
      case Children    => tagChildren  map Children   .apply
      case Parents     => tagParents   map Parents    .apply
    }
  }

  object createUseCaseGD extends GenericDataOptionGen(UseCaseGD) {
    import gd._
    override def valueFor(a: Attr) = a match {
      case Codes      => newReqCodeIdAndValue        .nes map Codes     .apply
      case CustomText => nonEmptyCustomTextMap map (_     map CustomText.apply)
      case ImpSrcs    => liveReqId             map (_.nes map ImpSrcs   .apply)
      case ImpTgts    => liveReqId             map (_.nes map ImpTgts   .apply)
      case Tags       => applicableTagId(Live) map (_.nes map Tags      .apply)
      case Title      => useCaseTitle1                    map Title     .apply
    }
  }

  def useCaseStepGD(uc: UseCase, step: UseCaseStep): Gen[UseCaseStepGD.NonEmptyValues] = {
    import UseCaseStepGD._

    lazy val gSteps =
      Gen.choose_!(uc.stepIterator.map(_.id)).set

    def gFlow(dir: Direction) =
      gSteps.map(ss => NonEmpty(SetDiff.xor(p.content.reqs.useCases.stepFlow(dir)(step.id), ss)))

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

  object savedViewGDv1 extends GenericDataGen(SavedViewGDv1) {
    import gd._
    import shipreq.webapp.member.project.data.savedview.Column
    import RandomData.savedViews._
    private val colNev = ColumnIGen(p.config.fields.customFields.keysIterator.map(Column.CustomField).toVector).columnNEV
    val genColumns        = colNev
    val genFilter         = filter.valid.forProject(p).option
    val genFilterDead     = filterDead
    val genName           = savedViewName
    val genSortCriteria   = colNev.flatMap(sortCriteria)
    override def valueFor(a: Attr) = a match {
      case Columns    => genColumns      map Columns   .apply
      case Filter     => genFilter       map Filter    .apply
      case FilterDead => genFilterDead   map FilterDead.apply
      case Name       => genName         map Name      .apply
      case Order      => genSortCriteria map Order     .apply
    }
  }

  object savedViewGD extends GenericDataGen(SavedViewGD) {
    import gd._
    import shipreq.webapp.member.project.data.savedview.Column
    import RandomData.savedViews._
    private val colNev = ColumnIGen(p.config.fields.customFields.keysIterator.map(Column.CustomField).toVector).columnNEV
    val genColumns        = colNev
    val genFilter         = filter.valid.forProject(p).option
    val genFilterDead     = filterDead
    val genName           = savedViewName
    val genSortCriteria   = colNev.flatMap(sortCriteria)
    val genImpGraphConfig = impGraphConfigForProject(p).option
    override def valueFor(a: Attr) = a match {
      case Columns        => genColumns        map Columns       .apply
      case Filter         => genFilter         map Filter        .apply
      case FilterDead     => genFilterDead     map FilterDead    .apply
      case Name           => genName           map Name          .apply
      case Order          => genSortCriteria   map Order         .apply
      case ImpGraphConfig => genImpGraphConfig map ImpGraphConfig.apply
    }
  }

  lazy val savedViewId: Option[Gen[SavedView.Id]] =
    Gen.tryGenChoose(p.savedViewIterator.map(_.id))

  lazy val savedViewIdNonDefault: Option[Gen[SavedView.Id]] =
    p.savedViews.flatMap(svs => Gen.tryGenChoose(svs.nonDefault.keys))

  lazy val manualIssueId: Option[Gen[ManualIssueId]] =
    Gen.tryGenChoose(p.manualIssues.imap.keysIterator)

  def manualIssueText: Gen[Text.ManualIssue.NonEmptyText] =
    TextGen.manualIssueAtom(existingReqId, existingUseCaseStepId, existingReqCodeId, existingApplicableTagId)
      .text1(Text.ManualIssue)

  // -------------------------------------------------------------------------------------------------------------------

  def genFieldStaticAdd: Option[Gen[FieldStaticAdd]] =
    Gen.tryGenChoose(staticFieldsToAdd).map(_ map FieldStaticAdd)

  def genUseCaseStepCreate: Option[Gen[UseCaseStepCreate]] =
    liveUseCase.map(genUC =>
      for {
        id    <- nextUseCaseStepId
        uc    <- genUC
        field <- Gen choose_! StaticField.useCaseStepTrees.whole.filter(_.useCaseStepTree.get(uc).nonEmpty)
        tree  = field.useCaseStepTree.get(uc)
        loc   <- tree.genParentLocation
      } yield UseCaseStepCreate(id, uc.id, field, loc)
    )

  def genProjectTemplateApply: Option[Gen[ProjectTemplateApply]] =
    if (p eq emptyState)
      Some(RandomData.events.genProjectTemplateApply)
    else
      None

  def genApplicableTagCreate: Gen[ApplicableTagCreate] =
    Gen.apply2(ApplicableTagCreate)(nextApplicableTagId, applicableTagGD.allValues)

  def genApplicableTagCreateV1: Gen[ApplicableTagCreateV1] =
    Gen.apply2(ApplicableTagCreateV1)(nextApplicableTagId, applicableTagGDv1.allValues)

  def genFieldCustomImpCreateV1: Option[Gen[FieldCustomImpCreateV1]] =
    customImpFieldGDv1.allValues.map(vs =>
      Gen.apply2(FieldCustomImpCreateV1)(nextCustomFieldImplicationId, vs))

  def genFieldCustomImpCreate: Option[Gen[FieldCustomImpCreate]] = {

    def reqTypesUsedInFields: Set[ReqTypeId] =
      cfg.fields.customImpFields.map(_.reqTypeId).toSet

    def liveReqTypes: Iterator[ReqTypeId] =
      StaticReqType.values.iterator.map(_.reqTypeId) ++
        cfg.reqTypes.custom.valuesIterator.filter(_.live is Live).map(_.id)

    for {
      genReqTypeId <- Gen.tryGenChoose(liveReqTypes.filterNot(reqTypesUsedInFields.contains))
      genVS        <- customImpFieldGD.allValues
    } yield for {
      id        <- nextCustomFieldImplicationId
      reqTypeId <- genReqTypeId
      vs        <- genVS
    } yield FieldCustomImpCreate(id, reqTypeId, vs)
  }

  def genCustomIssueTypeCreate: Gen[CustomIssueTypeCreate] =
    Gen.apply2(CustomIssueTypeCreate)(nextCustomIssueTypeId, customIssueTypeGD.allValues)

  def genCustomReqTypeCreateV1: Gen[CustomReqTypeCreateV1] =
    Gen.apply2(CustomReqTypeCreateV1)(nextCustomReqTypeId, customReqTypeGDv1.allValues)

  def genCustomReqTypeCreate: Gen[CustomReqTypeCreate] =
    Gen.apply2(CustomReqTypeCreate)(nextCustomReqTypeId, customReqTypeGD.allValues)

  def genFieldCustomTagCreateV1: Option[Gen[FieldCustomTagCreateV1]] =
    customTagFieldGDv1.allValues.map(vs =>
      Gen.apply2(FieldCustomTagCreateV1)(nextCustomFieldTagId, vs))

  def genFieldCustomTagCreate: Option[Gen[FieldCustomTagCreate]] = {

    def tagsUsedInFields: Set[TagId] =
      cfg.fields.customTagFields.map(_.tagId).toSet

    def liveTagIds: Iterator[TagGroupId] =
      p.config.tags.tree.valuesIterator.map(_.tag).filterSubType[TagGroup].filter(_.live is Live).map(_.id)

    for {
      genTagId <- Gen.tryGenChoose(liveTagIds.filterNot(tagsUsedInFields.contains))
      genVS    <- customTagFieldGD.allValues
    } yield for {
      id    <- nextCustomFieldTagId
      tagId <- genTagId
      vs    <- genVS
    } yield FieldCustomTagCreate(id, tagId, vs)
  }

  def genFieldCustomTextCreateV1: Gen[FieldCustomTextCreateV1] =
    Gen.apply2(FieldCustomTextCreateV1)(nextCustomFieldTextId, customTextFieldGDv1.allValues)

  def genFieldCustomTextCreate: Gen[FieldCustomTextCreate] =
    Gen.apply2(FieldCustomTextCreate)(nextCustomFieldTextId, customTextFieldGD.allValues)

  def genGenericReqCreate: Option[Gen[GenericReqCreate]] =
    for {
      reqTypeId <- customReqTypeId(Live)
      //vs        <- createGenericReqGD.allPossibleValues
    } yield
      Gen.apply3(GenericReqCreate)(nextGenericReqId, reqTypeId, createGenericReqGD.values)

  def genCodeGroupCreate: Gen[CodeGroupCreate] =
    Gen.apply2(CodeGroupCreate)(nextReqCodeIdG, codeGroupGD.allValues)

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

  def genCustomReqTypeDeleteHard: Option[Gen[CustomReqTypeDeleteHard]] =
    customReqTypeId(Live).map(_ map CustomReqTypeDeleteHard)

  def genCustomReqTypeDeleteSoft: Option[Gen[CustomReqTypeDeleteSoft]] =
    customReqTypeId(Live).map(_ map CustomReqTypeDeleteSoft)

  def genCodeGroupsDelete: Option[Gen[CodeGroupsDelete]] =
    codeGroupId(Live).map(_.nes map CodeGroupsDelete)

  def genCustomIssueTypeRestore: Option[Gen[CustomIssueTypeRestore]] =
    customIssueTypeId(Dead).map(_ map CustomIssueTypeRestore)

  def genCustomReqTypeRestore: Option[Gen[CustomReqTypeRestore]] =
    customReqTypeId(Dead).map(_ map CustomReqTypeRestore)

  def genReqsDelete: Option[Gen[ReqsDelete]] =
    liveReqId.map(reqId =>
      Gen.apply3(ReqsDelete.apply)(reqId.nes, codeGroupId(Live).setE, deletionReason))

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
        RandomBaseData.dir.flatMap { dir =>
          gReqId.flatMap { id =>
            Gen.choose_!(liveReqIds.filter(_ !=* id)).set1.map { ids =>
              val sd = SetDiff.xor(p.content.implications(dir)(id), ids)
              ReqImplicationsPatch(id, dir, NonEmpty force sd)
            }
          }
        }
      }

  def genReqCodesPatch: Option[Gen[ReqCodesPatch]] =
    liveReqId.map(gReqId =>
      for {
        reqId          <- gReqId
        inactiveValues = p.content.reqCodes.inactiveIdsByReqId(reqId)
        restore        <- Gen.tryGenChoose(inactiveValues.toVector).setE(0 to 2)
        activeValues   = p.content.reqCodes.activeReqCodesByReqId(reqId)
        activeIds      = activeValues.iterator.map(p.content.reqCodes.need(_).activeId.get).collect {case i: ApReqCodeId => i}.toSet
        remove         <- Gen.tryGenChoose(activeIds.toVector).setE(0 to 2)
        renameIds      <- Gen.tryGenChoose(remove.toVector).setE(0 to 2)
        addMin         = if (remove.nonEmpty || restore.nonEmpty) 0 else 1
        addIds         <- nextReqCodeIdA.list(addMin to 2)
        add            <- Gen sequence (addIds ++ renameIds).map(id => reqCode.value.strengthR(Set.empty[ApReqCodeId] + id))
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
      val sd = SetDiff.xor(p.content.reqTags(id), tags.whole)
      ReqTagsPatch(id, NonEmpty force sd)
    }

  def genFieldReposition: Gen[FieldReposition] =
    Gen.lazily {
      val order = p.config.fields.order
      // if (order.length < 2) // We always have at least 2 because of static fields
      val anyField = Gen.choose_!(order)
      for {
        id     <- anyField
        curPos = RelPos.get(order, id)
        tmp    = order.filterNot(f => (f ==* id) || curPos.exists(f ==* _)).map(_.some)
        tmp2   = if (curPos.isEmpty) tmp else tmp :+ None
        pos    <- Gen.choose_!(tmp2)
      } yield FieldReposition(id, pos)
    }

  def genContentRestore: Option[Gen[ContentRestore]] = {
    val restorableReqIds = Gen.tryGenChoose[ReqId](
      p.content.reqs.reqIterator().filter {
        case g: GenericReq => (g.liveExplicitly is Dead) && (g.copy(liveExplicitly = Live).live(cfg.reqTypes) is Live)
        case u: UseCase    => (u.liveExplicitly is Dead) && (u.copy(liveExplicitly = Live).live(cfg.reqTypes) is Live)
      }.map(_.id).toVector)
    if (restorableReqIds.isEmpty && codeGroupId(Dead).isEmpty)
      None
    else Some {
      val idSet = restorableReqIds.setE
      val codeSet = codeGroupId(Dead).setE
      Gen.apply2(ContentRestore)(idSet, codeSet).flatMap(cmd =>
        if (cmd.reqs.nonEmpty || cmd.codeGroups.nonEmpty)
          Gen pure cmd
        else if (restorableReqIds.isDefined)
          restorableReqIds.get.nes.map(ids => ContentRestore(ids.whole, Set.empty))
        else
          codeGroupId(Dead).get.nes.map(ids => ContentRestore(Set.empty, ids.whole))
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

  def genApplicableTagUpdateV1: Option[Gen[ApplicableTagUpdateV1]] =
    applicableTagId(Live).map(gId =>
      for {
        id <- gId
        vs <- applicableTagGDv1.nonEmptyValues
      } yield {
        import ApplicableTagGDv1._
        ApplicableTagUpdateV1(id, NonEmpty.force(emptyValues ++ vs.valuesIterator.map {
          case ValueForParents(v) => ValueForParents(v - id)
          case ValueForChildren(v) => ValueForChildren(v.filterNot(_ ==* id))
          case v => v
        }))
      }
    )

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

  def genFieldCustomImpUpdateV1: Option[Gen[FieldCustomImpUpdateV1]] =
    for {
      id <- customFieldImpId(Live)
      vs <- customImpFieldGDv1.nonEmptyValues
    } yield
      Gen.apply2(FieldCustomImpUpdateV1)(id, vs)

  def genFieldCustomImpUpdate: Option[Gen[FieldCustomImpUpdate]] =
    for {
      id <- customFieldImpId(Live)
      vs <- customImpFieldGD.nonEmptyValues
    } yield
      Gen.apply2(FieldCustomImpUpdate)(id, vs)

  def genCustomIssueTypeUpdate: Option[Gen[CustomIssueTypeUpdate]] =
    customIssueTypeId(Live).map(id =>
      Gen.apply2(CustomIssueTypeUpdate)(id, customIssueTypeGD.nonEmptyValues))

  def genCustomReqTypeUpdateV1: Option[Gen[CustomReqTypeUpdateV1]] =
    customReqTypeId(Live).map(id =>
      Gen.apply2(CustomReqTypeUpdateV1)(id, customReqTypeGDv1.nonEmptyValues))

  def genCustomReqTypeUpdate: Option[Gen[CustomReqTypeUpdate]] =
    customReqTypeId(Live).map(id =>
      Gen.apply2(CustomReqTypeUpdate)(id, customReqTypeGD.nonEmptyValues))

  def genFieldCustomTagUpdateV1: Option[Gen[FieldCustomTagUpdateV1]] =
    for {
      id <- customFieldTagId(Live)
      vs <- customTagFieldGDv1.nonEmptyValues
    } yield
      Gen.apply2(FieldCustomTagUpdateV1)(id, vs)

  def genFieldCustomTagUpdate: Option[Gen[FieldCustomTagUpdate]] =
    for {
      id <- customFieldTagId(Live)
      vs <- customTagFieldGD.nonEmptyValues
    } yield
      Gen.apply2(FieldCustomTagUpdate)(id, vs)

  def genFieldCustomTextUpdateV1: Option[Gen[FieldCustomTextUpdateV1]] =
    customFieldTextId(Live).map(id =>
      Gen.apply2(FieldCustomTextUpdateV1)(id, customTextFieldGDv1.nonEmptyValues))

  def genFieldCustomTextUpdate: Option[Gen[FieldCustomTextUpdate]] =
    customFieldTextId(Live).map(id =>
      Gen.apply2(FieldCustomTextUpdate)(id, customTextFieldGD.nonEmptyValues))

  def genCodeGroupUpdate: Option[Gen[CodeGroupUpdate]] =
    codeGroupId(Live).map(id =>
      Gen.apply2(CodeGroupUpdate)(id, codeGroupGD.nonEmptyValues))

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

  def genSavedViewCreateV1: Gen[SavedViewCreateV1] =
      Gen.apply6(SavedViewCreateV1)(
        nextSavedViewId,
        savedViewGD.genName,
        savedViewGD.genColumns,
        savedViewGD.genSortCriteria,
        savedViewGD.genFilterDead,
        savedViewGD.genFilter)

  def genSavedViewCreate: Gen[SavedViewCreate] =
      Gen.apply7(SavedViewCreate)(
        nextSavedViewId,
        savedViewGD.genName,
        savedViewGD.genColumns,
        savedViewGD.genSortCriteria,
        savedViewGD.genFilterDead,
        savedViewGD.genFilter,
        savedViewGD.genImpGraphConfig)

  def genSavedViewUpdateV1: Option[Gen[SavedViewUpdateV1]] =
    savedViewId.map(Gen.apply2(SavedViewUpdateV1)(_, savedViewGDv1.nonEmptyValues))

  def genSavedViewUpdate: Option[Gen[SavedViewUpdate]] =
    savedViewId.map(Gen.apply2(SavedViewUpdate)(_, savedViewGD.nonEmptyValues))

  def genSavedViewDefaultSet: Option[Gen[SavedViewDefaultSet]] =
    savedViewIdNonDefault.map(_ map SavedViewDefaultSet)

  def genSavedViewDelete: Option[Gen[SavedViewDelete]] =
    savedViewId.map(_ map SavedViewDelete)

  def genManualIssueCreate: Gen[ManualIssueCreate] =
    manualIssueText.map(ManualIssueCreate(p.manualIssues.nextId, _))

  def genManualIssueUpdate: Option[Gen[ManualIssueUpdate]] =
    manualIssueId.map(Gen.apply2(ManualIssueUpdate)(_, manualIssueText))

  def genManualIssueDelete: Option[Gen[ManualIssueDelete]] =
    manualIssueId.map(_ map ManualIssueDelete)

  def genAccessUpdate: Option[Gen[AccessUpdate]] = {
    type Entries = Vector[(UserId.Public, Option[ProjectRole])]

    val access = p.access.asMap

    val newUserGen: Option[Gen[UserId.Public]] =
      config.userIds match {
        case Some(ids) => NonEmptySet.option(ids.whole -- access.keySet).map(Gen.chooseNE(_))
        case None      => Some(RandomData.userIdPublic)
      }

    def update: Gen[Entries] =
      Gen.subset1(access.keys.toVector).flatMap { ids =>
        Gen.traverse(ids) { id =>
          Gen.choose_!(RandomEventStream.projectRoleOptions - access.get(id))
            .map((id, _))
        }
      }

    newUserGen match {

      case Some(genNewUser) =>
        // Can add new users

        def add: Gen[Entries] =
          (genNewUser & projectRole.map(Some(_))).vector(1 to 4)

        val entries: Gen[Entries] =
          Gen.chooseInt(3).flatMap {
            case 0 => update
            case 1 => add
            case _ => Gen.lift2(update, add)(_ ++ _)
          }

        Some(entries.flatMap { es =>
          val m = es.toMap
          val u = ProjectAccess.empty.update(m)
          if (u.hasAdmin)
            Gen pure AccessUpdate(m)
          else
            genNewUser.map(u => AccessUpdate(m.updated(u, Some(ProjectRole.Admin))))
        })

      case None =>
        // Can't add new users
        Option.when(access.sizeIs >= 2) {
          update.map { es =>
            val m = es.toMap
            val u = ProjectAccess.empty.update(m)
            if (u.hasAdmin)
              AccessUpdate(m)
            else {
              val existingAdmin = access.find(_._2 ==* ProjectRole.Admin).get._1
              AccessUpdate(m - existingAdmin)
            }
          }
        }
    }
  }

  private val possibleActiveEventGensWithNames: NonEmptyVector[(EventName, Option[Gen[ActiveEvent]])] =
    valuesForAdt[ActiveEvent, (EventName, Option[Gen[ActiveEvent]])] {
      // Note: not using [case e: Xxx => EventName(e) -> xxx] here because the valuesForAdt doesn't like it
      case _: AccessUpdate            => EventName("AccessUpdate"           ) -> genAccessUpdate
      case _: ApplicableTagCreate     => EventName("ApplicableTagCreate"    ) -> genApplicableTagCreate
      case _: ApplicableTagUpdate     => EventName("ApplicableTagUpdate"    ) -> genApplicableTagUpdate
      case _: ContentRestore          => EventName("ContentRestore"         ) -> genContentRestore
      case _: CustomIssueTypeCreate   => EventName("CustomIssueTypeCreate"  ) -> genCustomIssueTypeCreate
      case _: CustomIssueTypeDelete   => EventName("CustomIssueTypeDelete"  ) -> genCustomIssueTypeDelete
      case _: CustomIssueTypeRestore  => EventName("CustomIssueTypeRestore" ) -> genCustomIssueTypeRestore
      case _: CustomIssueTypeUpdate   => EventName("CustomIssueTypeUpdate"  ) -> genCustomIssueTypeUpdate
      case _: CustomReqTypeCreate     => EventName("CustomReqTypeCreate"    ) -> genCustomReqTypeCreate
      case _: CustomReqTypeDeleteHard => EventName("CustomReqTypeDeleteHard") -> genCustomReqTypeDeleteHard
      case _: CustomReqTypeDeleteSoft => EventName("CustomReqTypeDeleteSoft") -> genCustomReqTypeDeleteSoft
      case _: CustomReqTypeRestore    => EventName("CustomReqTypeRestore"   ) -> genCustomReqTypeRestore
      case _: CustomReqTypeUpdate     => EventName("CustomReqTypeUpdate"    ) -> genCustomReqTypeUpdate
      case _: FieldCustomDelete       => EventName("FieldCustomDelete"      ) -> genFieldCustomDelete
      case _: FieldCustomImpCreate    => EventName("FieldCustomImpCreate"   ) -> genFieldCustomImpCreate
      case _: FieldCustomImpUpdate    => EventName("FieldCustomImpUpdate"   ) -> genFieldCustomImpUpdate
      case _: FieldCustomRestore      => EventName("FieldCustomRestore"     ) -> genFieldCustomRestore
      case _: FieldCustomTagCreate    => EventName("FieldCustomTagCreate"   ) -> genFieldCustomTagCreate
      case _: FieldCustomTagUpdate    => EventName("FieldCustomTagUpdate"   ) -> genFieldCustomTagUpdate
      case _: FieldCustomTextCreate   => EventName("FieldCustomTextCreate"  ) -> genFieldCustomTextCreate
      case _: FieldCustomTextUpdate   => EventName("FieldCustomTextUpdate"  ) -> genFieldCustomTextUpdate
      case _: FieldReposition         => EventName("FieldReposition"        ) -> genFieldReposition
      case _: FieldStaticAdd          => EventName("FieldStaticAdd"         ) -> genFieldStaticAdd
      case _: FieldStaticRemove       => EventName("FieldStaticRemove"      ) -> genFieldStaticRemove
      case _: GenericReqCreate        => EventName("GenericReqCreate"       ) -> genGenericReqCreate
      case _: GenericReqTitleSet      => EventName("GenericReqTitleSet"     ) -> genGenericReqTitleSet
      case _: GenericReqTypeSet       => EventName("GenericReqTypeSet"      ) -> genGenericReqTypeSet
      case _: ManualIssueCreate       => EventName("ManualIssueCreate"      ) -> genManualIssueCreate
      case _: ManualIssueDelete       => EventName("ManualIssueDelete"      ) -> genManualIssueDelete
      case _: ManualIssueUpdate       => EventName("ManualIssueUpdate"      ) -> genManualIssueUpdate
      case _: ProjectNameSet          => EventName("ProjectNameSet"         ) -> genProjectNameSet
      case _: ProjectTemplateApply    => EventName("ProjectTemplateApply"   ) -> genProjectTemplateApply
      case _: CodeGroupCreate         => EventName("CodeGroupCreate"        ) -> genCodeGroupCreate
      case _: CodeGroupsDelete        => EventName("CodeGroupsDelete"       ) -> genCodeGroupsDelete
      case _: CodeGroupUpdate         => EventName("CodeGroupUpdate"        ) -> genCodeGroupUpdate
      case _: ReqCodesPatch           => EventName("ReqCodesPatch"          ) -> genReqCodesPatch
      case _: ReqFieldCustomTextSet   => EventName("ReqFieldCustomTextSet"  ) -> genReqFieldCustomTextSet
      case _: ReqImplicationsPatch    => EventName("ReqImplicationsPatch"   ) -> genReqImplicationsPatch
      case _: ReqsDelete              => EventName("ReqsDelete"             ) -> genReqsDelete
      case _: ReqTagsPatch            => EventName("ReqTagsPatch"           ) -> genReqTagsPatch
      case _: SavedViewCreate         => EventName("SavedViewCreate"        ) -> genSavedViewCreate
      case _: SavedViewDefaultSet     => EventName("SavedViewDefaultSet"    ) -> genSavedViewDefaultSet
      case _: SavedViewDelete         => EventName("SavedViewDelete"        ) -> genSavedViewDelete
      case _: SavedViewUpdate         => EventName("SavedViewUpdate"        ) -> genSavedViewUpdate
      case _: TagDelete               => EventName("TagDelete"              ) -> genTagDelete
      case _: TagGroupCreate          => EventName("TagGroupCreate"         ) -> genTagGroupCreate
      case _: TagGroupUpdate          => EventName("TagGroupUpdate"         ) -> genTagGroupUpdate
      case _: TagRestore              => EventName("TagRestore"             ) -> genTagRestore
      case _: UseCaseCreate           => EventName("UseCaseCreate"          ) -> genUseCaseCreate
      case _: UseCaseStepCreate       => EventName("UseCaseStepCreate"      ) -> genUseCaseStepCreate
      case _: UseCaseStepDelete       => EventName("UseCaseStepDelete"      ) -> genUseCaseStepDelete
      case _: UseCaseStepRestore      => EventName("UseCaseStepRestore"     ) -> genUseCaseStepRestore
      case _: UseCaseStepShiftLeft    => EventName("UseCaseStepShiftLeft"   ) -> genUseCaseStepShiftLeft
      case _: UseCaseStepShiftRight   => EventName("UseCaseStepShiftRight"  ) -> genUseCaseStepShiftRight
      case _: UseCaseStepUpdate       => EventName("UseCaseStepUpdate"      ) -> genUseCaseStepUpdate
      case _: UseCaseTitleSet         => EventName("UseCaseTitleSet"        ) -> genUseCaseTitleSet
    }

  private def possibleRetiredEventGensWithNames: NonEmptyVector[(EventName, Option[Gen[RetiredEvent]])] =
    valuesForAdt[RetiredEvent, (EventName, Option[Gen[RetiredEvent]])] {
      // Note: not using [case e: Xxx => EventName(e) -> xxx] here because the valuesForAdt doesn't like it
      case _: ApplicableTagCreateV1   => EventName("ApplicableTagCreateV1"  ) -> genApplicableTagCreateV1
      case _: ApplicableTagUpdateV1   => EventName("ApplicableTagUpdateV1"  ) -> genApplicableTagUpdateV1
      case _: CustomReqTypeCreateV1   => EventName("CustomReqTypeCreateV1"  ) -> genCustomReqTypeCreateV1
      case _: CustomReqTypeUpdateV1   => EventName("CustomReqTypeUpdateV1"  ) -> genCustomReqTypeUpdateV1
      case _: CustomReqTypeDelete     => EventName("CustomReqTypeDelete"    ) -> genCustomReqTypeDelete
      case _: FieldCustomImpCreateV1  => EventName("FieldCustomImpCreateV1" ) -> genFieldCustomImpCreateV1
      case _: FieldCustomImpUpdateV1  => EventName("FieldCustomImpUpdateV1" ) -> genFieldCustomImpUpdateV1
      case _: FieldCustomTagCreateV1  => EventName("FieldCustomTagCreateV1" ) -> genFieldCustomTagCreateV1
      case _: FieldCustomTagUpdateV1  => EventName("FieldCustomTagUpdateV1" ) -> genFieldCustomTagUpdateV1
      case _: FieldCustomTextCreateV1 => EventName("FieldCustomTextCreateV1") -> genFieldCustomTextCreateV1
      case _: FieldCustomTextUpdateV1 => EventName("FieldCustomTextUpdateV1") -> genFieldCustomTextUpdateV1
      case _: SavedViewCreateV1       => EventName("SavedViewCreateV1"      ) -> genSavedViewCreateV1
      case _: SavedViewUpdateV1       => EventName("SavedViewUpdateV1"      ) -> genSavedViewUpdateV1
    }

  private val possibleEventGensWithNames: NonEmptyVector[(EventName, Option[Gen[Event]])] = {
    var es =
      if (config.retiredEvents)
        possibleActiveEventGensWithNames ++ possibleRetiredEventGensWithNames
      else
        possibleActiveEventGensWithNames
    if (!config.reqCodeEvents)
      es = NonEmptyVector.force(es.whole.filterNot(_._1.value.contains("Code")))
    es
  }

  private val possibleEventGens: NonEmptyVector[Option[Gen[Event]]] =
    possibleEventGensWithNames.map(_._2)

  val eventGens: NonEmptyVector[Gen[Event]] =
    NonEmptyVector force possibleEventGens.iterator.filterDefined.toVector

  val eventGen: Gen[Event] =
    Gen chooseGenNE eventGens

  def eventGenOfTypes(types: NonEmptySet[EventName]): Gen[Event] = {
    val gens = possibleEventGensWithNames.iterator.filter(x => types.contains(x._1)).map(_._2).filterDefined.toVector
    if (gens.isEmpty)
      ErrorMsg(s"No event gens possible for types: ${types.whole.map(_.value).mkString(", ")}").throwException()
    else
      Gen.chooseGen_!(gens)
  }

//  def applyEventSG[S](observe: ObserveFn[S]): StateGen[S, (Event, Project)] =
//    StateGen.tailrec[S, (Event, Project)](s =>
//      eventGen.map { e =>
//        val r = ApplyEvent.untrusted(e)(p)
//        val s2 = observe(s, e, r)
//        r.bimap(_ => s2, p2 => (s2, (e, p2)))
//      }
//    )

  def applicableEventS[S](init: S)(observe: ObserveFn[S]): Gen[((S, Project), Event)] = {
    import Project.Equality.WithHistoryByOrd._
    Monad[Gen].tailRecM(init)(s =>
      eventGen.map { e =>
        var r = ApplyEvent.untrusted.partialApplyUnverified(e)(p)
        r foreach { p2 => if (p === p2) r = -\/(ErrorMsg("No change")) }
        val s2 = observe(s, e, r)
        r.bimap(_ => s2, p2 => ((s2, p2), e))
      }
    )
  }

  def verifiedEvent: Gen[(State, VerifiedEvent)] =
    RandomEventStream.liftGE(eventGen).run(curState)
}
