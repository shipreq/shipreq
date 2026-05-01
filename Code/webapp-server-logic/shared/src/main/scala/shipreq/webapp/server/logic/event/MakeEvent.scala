package shipreq.webapp.server.logic.event

import japgolly.microlibs.nonempty.NonEmpty
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.util.PotentialChange._
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.member.project.data.DataImplicits._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event.Event._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.WsReqRes.{ProjectNameSet => _, _}
import shipreq.webapp.member.project.protocol.websocket._
import shipreq.webapp.member.project.text.PlainText
import shipreq.webapp.member.project.util.GenericDataMacros._

/**
 * Translates commands inputs into [[ActiveEvent]]s.
 */
object MakeEvent {

  type Result = PotentialChange[ErrorMsg, ActiveEvent]

  // ===================================================================================================================

  @inline private implicit class DisjExt[A](private val v: ErrorMsg \/ A) extends AnyVal {
    @inline def toMakeEventResult(f: A => Result): Result =
      v.fold(Failure(_), f)
  }

  private def eventIfNonEmpty[A](a: A)(f: NonEmpty[A] => Result)(implicit proof: NonEmpty.ProofMono[A]): Result =
    NonEmpty(a) match {
      case Some(b) => f(b)
      case None    => Unchanged
    }

  @inline private implicit def autoSuccess(e: ActiveEvent) = Success(e)

  private def fail(s: String) = Failure(ErrorMsg(s))

  // ===================================================================================================================

  def updateAccess(cmd: UpdateAccessCmd.Modify, project: Project): Result = {
    val access = project.access.asMap
    val updates = cmd.updates.filterNot { case (u, o) => access.get(u) ==* o }
    if (updates.isEmpty)
      Unchanged
    else
      Event.AccessUpdate(updates)
  }

  def projectNameSetFn(name: String): Result =
    ProjectNameSet(name)

  def reqTypeImplicationMod(input: ReqTypeImplicationMod.RequestType): Result = {
    val (id, imp) = input
    CustomReqTypeUpdate(id, CustomReqTypeGD.Implication(imp))
  }

  def updateConfig(cmd: UpdateConfigCmd, project: Project): Result = {
    def nextId = project.idCeilings.customField + 1

    cmd match {

      case cmd: UpdateConfigCmd.CustomIssueTypeCreate =>
        val id = CustomIssueTypeId(project.idCeilings.customIssueType + 1)
        import cmd._
        val values = gdAllValues(CustomIssueTypeGD , "")
        CustomIssueTypeCreate(id, values)

      case UpdateConfigCmd.CustomIssueTypeUpdate(id, vs) =>
        project.config.customIssueTypes.attempt(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues2(CustomIssueTypeGD, cur, vs)
          eventIfNonEmpty(vs2)(CustomIssueTypeUpdate(id, _))
        }

      case UpdateConfigCmd.CustomIssueTypeDelete(id) =>
        CustomIssueTypeDelete(id)

      case UpdateConfigCmd.CustomIssueTypeRestore(id) =>
        CustomIssueTypeRestore(id)

      case cmd: UpdateConfigCmd.CustomReqTypeCreate =>
        val id = CustomReqTypeId(project.idCeilings.customReqType + 1)
        import cmd._
        val values = gdAllValues(CustomReqTypeGD , "")
        CustomReqTypeCreate(id, values)

      case UpdateConfigCmd.CustomReqTypeUpdate(id, vs) =>
        project.config.reqTypes.get(id) match {
          case Some(cur: CustomReqType) =>
            val vs2 = gdUnequalValues2(CustomReqTypeGD, cur, vs)
            eventIfNonEmpty(vs2)(CustomReqTypeUpdate(id, _))
          case Some(f) => fail(s"$f must be a CustomReqType.")
          case None    => fail(s"$id not found")
        }

      case UpdateConfigCmd.CustomReqTypeDeleteHard(id) =>
        CustomReqTypeDeleteHard(id)

      case UpdateConfigCmd.CustomReqTypeDeleteSoft(id) =>
        CustomReqTypeDeleteSoft(id)

      case UpdateConfigCmd.CustomReqTypeRestore(id) =>
        CustomReqTypeRestore(id)

      case UpdateConfigCmd.FieldUpdateOrder(id, pos) =>
        FieldReposition(id, pos)

      case c: UpdateConfigCmd.CustomFieldCreateImp =>
        val id = CustomField.Implication.Id(nextId)
        FieldCustomImpCreate(id, c.reqTypeId, gdAllValues(CustomImpFieldGD, "c"))

      case c: UpdateConfigCmd.CustomFieldCreateTag =>
        val id = CustomField.Tag.Id(nextId)
        FieldCustomTagCreate(id, c.tagId, gdAllValues(CustomTagFieldGD, "c"))

      case c: UpdateConfigCmd.CustomFieldCreateText =>
        locally(c) // used by macros
        val id = CustomField.Text.Id(nextId)
        FieldCustomTextCreate(id, gdAllValues(CustomTextFieldGD, "c"))

      case UpdateConfigCmd.CustomFieldUpdateImp(id, vs) =>
        locally(vs) // used by macros
        project.config.fields.customAttempt(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues2(CustomImpFieldGD, cur, vs)
          eventIfNonEmpty(vs2)(FieldCustomImpUpdate(id, _))
        }

      case UpdateConfigCmd.CustomFieldUpdateTag(id, vs) =>
        project.config.fields.customAttempt(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues2(CustomTagFieldGD, cur, vs)
          eventIfNonEmpty(vs2)(FieldCustomTagUpdate(id, _))
        }

      case UpdateConfigCmd.CustomFieldUpdateText(id, vs) =>
        project.config.fields.customAttempt(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues2(CustomTextFieldGD, cur, vs)
          eventIfNonEmpty(vs2)(FieldCustomTextUpdate(id, _))
        }

      case UpdateConfigCmd.StaticFieldRemove(f) =>
        FieldStaticRemove(f)

      case UpdateConfigCmd.StaticFieldAdd(f) =>
        FieldStaticAdd(f)

      case UpdateConfigCmd.CustomFieldDelete(id) =>
        FieldCustomDelete(id)

      case UpdateConfigCmd.CustomFieldRestore(id) =>
        FieldCustomRestore(id)

      case t: UpdateConfigCmd.ToModifyTags =>
        tagCrud(t, project)
    }
  }

  private final class TagChildrenHelper[T <: TagId](project: Project, tagId: TagId, select: PartialFunction[TagId, T]) {
    val allChildren = project.config.tags.directChildren(tagId)

    val okChildren: Vector[T] =
      allChildren
        .iterator
        .map(select.lift)
        .filterDefined
        .toVector

    val okChildrenSet: Set[TagId] =
      okChildren.toSet

    val otherChildren: Vector[TagId] =
      allChildren.filterNot(okChildrenSet.contains)
  }

  private object TagChildrenHelper {
    def liveApTags(project: Project, tagId: TagId): TagChildrenHelper[ApplicableTagId] =
      new TagChildrenHelper(project, tagId, {
        case id: ApplicableTagId if project.config.tags.needApplicableTag(id).live.is(Live) => id
      })

    def liveTags(project: Project, tagId: TagId): TagChildrenHelper[TagId] =
      new TagChildrenHelper(project, tagId, {
        case id if project.config.tags.tree.need(id).tag.live.is(Live) => id
      })
  }

  private final class TagParentsHelper[T <: TagId](project: Project, tagId: TagId, select: TagId => Option[T]) {
    val allParents = project.config.tags.parents(tagId)

    val okParents: Map[T, Option[TagId]] =
      allParents
        .iterator
        .map { case (id, pos) => select(id).map((_, pos)) }
        .filterDefined
        .toMap

    val okParentsSet: Set[T] =
      okParents.keySet

    private val _okParentsSet: Set[TagId] =
      okParentsSet.asInstanceOf[Set[TagId]]

    val otherParents: Map[TagId, Option[TagId]] =
      allParents.view.filterKeys(!_okParentsSet.contains(_)).toMap
  }

  private object TagParentsHelper {

    @inline def pf[T <: TagId](project: Project, tagId: TagId, select: PartialFunction[TagId, T]): TagParentsHelper[T] =
      new TagParentsHelper(project, tagId, select.lift)

    def liveTagGroups(project: Project, tagId: TagId): TagParentsHelper[TagGroupId] =
      pf(project, tagId, {
        case id: TagGroupId if project.config.tags.needTagGroup(id).live.is(Live) => id
      })
  }

  private def tagCrud(cmd: UpdateConfigCmd.ToModifyTags, project: Project): Result = {
    def nextId = project.idCeilings.tag + 1
    cmd match {

      case UpdateConfigCmd.ApplicableTagCreate(newValues) =>
        val id = ApplicableTagId(nextId)
        ApplicableTagCreate(id, newValues)

      case UpdateConfigCmd.ApplicableTagUpdate(id, newValues) =>
        PotentialChange.fromDisjunction(project.config.tags.applicableTag(id)).flatMap { tag =>
          import ApplicableTagGD._

          if (newValues.containsK(Children))
            fail("You cannot specify ApplicableTag children")
          else if (Parents.get(newValues).exists(_.value.keysIterator.exists(project.config.tags.tree.need(_).tag.live is Dead)))
            fail("You cannot specify dead parents")
          else if (ApplicableReqTypes.get(newValues).exists(_.value.reqTypes.exists(project.config.reqTypes.live(_, Dead) is Dead)))
            fail("You cannot specify dead req types")
          else {

            // Update values if necessary, and remove unchanged
            val b = valueBuilder()
            newValues.valuesIterator.foreach {
              case ValueForChildren(_) => () // prevented above
              case ValueForColour  (v) => b.addIfChanged(Colour)(tag.colour, v)
              case ValueForDesc    (v) => b.addIfChanged(Desc  )(tag.desc  , v)
              case ValueForKey     (v) => b.addIfChanged(Key   )(tag.key   , v)

              case ValueForApplicableReqTypes(v) =>
                val v2 = v.withDeadFrom(tag.applicableReqTypes, project.config.reqTypes)
                b.addIfChanged(ApplicableReqTypes)(tag.applicableReqTypes, v2)

              case ValueForParents(v) =>
                val h = TagParentsHelper.liveTagGroups(project, id)
                val v2 = h.otherParents ++ v
                b.addIfChanged(Parents)(h.allParents, v2)
            }

            eventIfNonEmpty(b.values())(ApplicableTagUpdate(id, _))
          }
        }

      case UpdateConfigCmd.TagGroupCreate(newValues) =>
        val id = TagGroupId(nextId)
        TagGroupCreate(id, newValues)

      case UpdateConfigCmd.TagGroupUpdate(id, newValues) =>
        PotentialChange.fromDisjunction(project.config.tags.tagGroup(id)).flatMap { tag =>
          import TagGroupGD._

          if (Children.get(newValues).exists(_.value.exists(project.config.tags.tree.need(_).tag.live is Dead)))
            fail("You cannot specify dead children")
          else if (Parents.get(newValues).exists(_.value.keysIterator.exists(project.config.tags.tree.need(_).tag.live is Dead)))
            fail("You cannot specify dead parents")
          else {

            // Update values if necessary, and remove unchanged
            val b = valueBuilder()
            newValues.valuesIterator.foreach {
              case ValueForDesc       (v) => b.addIfChanged(Desc       )(tag.desc       , v)
              case ValueForExclusivity(v) => b.addIfChanged(Exclusivity)(tag.exclusivity, v)
              case ValueForName       (v) => b.addIfChanged(Name       )(tag.name       , v)

              case ValueForChildren(v) =>
                val h = TagChildrenHelper.liveTags(project, id)
                val v2 = h.otherChildren ++ v
                b.addIfChanged(Children)(h.allChildren, v2)

              case ValueForParents(v) =>
                val h = TagParentsHelper.liveTagGroups(project, id)
                val v2 = h.otherParents ++ v
                b.addIfChanged(Parents)(h.allParents, v2)
            }

            eventIfNonEmpty(b.values())(TagGroupUpdate(id, _))
          }
        }

      case UpdateConfigCmd.TagSetLiveChildrenOrder(tagId, newLiveChildrenOrder) =>
        val h = TagChildrenHelper.liveApTags(project, tagId)
        if (h.okChildren ==* newLiveChildrenOrder)
          Unchanged
        else if (h.okChildrenSet !=* newLiveChildrenOrder.toSet)
          fail("Tag group contains different children than specified. Please try again.")
        else {
          val newChildren = h.otherChildren ++ newLiveChildrenOrder
          val values = TagGroupGD.nev(TagGroupGD.ValueForChildren(newChildren))
          TagGroupUpdate(tagId, values)
        }

      case UpdateConfigCmd.TagDelete(id) =>
        TagDelete(id)

      case UpdateConfigCmd.TagRestore(id) =>
        TagRestore(id)
    }
  }

  private final class ReqCodeIdCounter(project: Project) {
    private var i = project.idCeilings.reqCode
    val ap    = () => {i += 1; ApReqCodeId   (i)}
    val group = () => {i += 1; ReqCodeGroupId(i)}
  }

  private def reqCodeIdCounter(project: Project) =
    new ReqCodeIdCounter(project)

  def createContent(cmd: CreateContentCmd, project: Project): Result = {
    val nextCodeId = reqCodeIdCounter(project)
    cmd match {
      case CreateContentCmd.CreateCodeGroup(code, title) =>
        locally(title) // used by macros

        def makeEvent(id: ReqCodeGroupId) =
          Success(CodeGroupCreate(id, gdAllValues(CodeGroupGD, "")))

        project.content.reqCodes.get(code) match {
          case None => makeEvent(nextCodeId.group())
          case Some(d) =>
            if (d.isActive)
              fail("Code in use.")
            else
              d.deadGroup match {
                case Some(dg) => makeEvent(dg.id)
                case None     => makeEvent(nextCodeId.group())
              }
        }

      case i: CreateContentCmd.CreateGenericReq =>
        var vs = GenericReqGD.emptyValues
        for (cs <- NonEmptySet.option(i.codes)) {
          // If a code is in use, ApplyEvent will catch it
          val v = cs.map(c => ApReqCodeId.AndValue(nextCodeId.ap(), c))
          vs += GenericReqGD.Codes(v)
        }
        for (v <- NonEmpty(i.customText))                vs += GenericReqGD.CustomText(v)
        for (v <- NonEmptySet.option(i.imps(Backwards))) vs += GenericReqGD.ImpSrcs(v)
        for (v <- NonEmptySet.option(i.imps(Forwards)))  vs += GenericReqGD.ImpTgts(v)
        for (v <- NonEmptySet.option(i.tags))            vs += GenericReqGD.Tags(v)
        for (v <- NonEmptyArraySeq.option(i.title))      vs += GenericReqGD.Title(v)
        val id = GenericReqId(project.idCeilings.req + 1)
        GenericReqCreate(id, i.reqType, vs)

      case i: CreateContentCmd.CreateUseCase =>
        var vs = UseCaseGD.emptyValues
        for (cs <- NonEmptySet.option(i.codes)) {
          // If a code is in use, ApplyEvent will catch it
          val v = cs.map(c => ApReqCodeId.AndValue(nextCodeId.ap(), c))
          vs += UseCaseGD.Codes(v)
        }
        for (v <- NonEmpty(i.customText))                vs += UseCaseGD.CustomText(v)
        for (v <- NonEmptySet.option(i.imps(Backwards))) vs += UseCaseGD.ImpSrcs(v)
        for (v <- NonEmptySet.option(i.imps(Forwards)))  vs += UseCaseGD.ImpTgts(v)
        for (v <- NonEmptySet.option(i.tags))            vs += UseCaseGD.Tags(v)
        for (v <- NonEmptyArraySeq.option(i.title))      vs += UseCaseGD.Title(v)
        val id = UseCaseId(project.idCeilings.req + 1)
        val stepId = UseCaseStepId(project.idCeilings.useCaseStep + 1)
        UseCaseCreate(id, stepId, vs)
    }
  }

  def updateContent(cmd: UpdateContentCmd, project: Project): Result =
    cmd match {
      case UpdateContentCmd.SetGenericReqTitle(id, v) =>
        GenericReqTitleSet(id, v)

      case UpdateContentCmd.SetUseCaseTitle(id, v) =>
        UseCaseTitleSet(id, v)

      case UpdateContentCmd.UpdateUseCaseStep(id, vs) =>
        UseCaseStepUpdate(id, vs)

      case UpdateContentCmd.PatchReqTags(id, v) =>
        ReqTagsPatch(id, v)

      case UpdateContentCmd.SetCustomTextField(id, f, v) =>
        ReqFieldCustomTextSet(id, f, v)

      case UpdateContentCmd.PatchImplications(id, dir, v) =>
        ReqImplicationsPatch(id, dir, v)

      case UpdateContentCmd.PatchReqCodes(reqId, cs) =>
        var remove : Set[ApReqCodeId]                          = UnivEq.emptySet
        var restore: Set[ApReqCodeId]                          = UnivEq.emptySet
        var add    : Multimap[ReqCode.Value, Set, ApReqCodeId] = UnivEq.emptySetMultimap
        var r      : Option[Result]                            = None

        def fail(err: String): Unit =
          r = Some(Failure(ErrorMsg(err)))

        import ReqCode._
        for (c <- cs.value.removed)
          project.content.reqCodes.get(c) match {
            case Some(a: ActiveReq) if a.reqId ==* reqId => remove += a.id
            case od if od.exists(_.isActive)             => fail(s"Cannot remove ${PlainText reqCode c}: Doesn't belong to $reqId.")
            case _                                       => fail(s"Cannot remove ${PlainText reqCode c}: Not found.")
          }

        if (r.isEmpty) {
          val nextCodeId = reqCodeIdCounter(project)
          for (c <- cs.value.added)
            project.content.reqCodes.get(c) match {
              case Some(d) if d.isActive =>
                Failure(s"Code in use: ${PlainText reqCode c}.")

              case od => od.flatMap(_.reqInactive(reqId).ifNonEmpty(_.min)) match {
                case None    => add = add.add(c, nextCodeId.ap())
                case Some(i) => restore += i
              }
            }
        }

        r getOrElse ReqCodesPatch(reqId, remove, restore, add)

      case UpdateContentCmd.SetGenericReqType(id, v) =>
        GenericReqTypeSet(id, v)

      case UpdateContentCmd.SetCodeGroupTitle(id, v) =>
        CodeGroupUpdate(id, CodeGroupGD.Title(v))

      case UpdateContentCmd.SetCodeGroupCode(id, v) =>
        CodeGroupUpdate(id, CodeGroupGD.Code(v))

      case UpdateContentCmd.DeleteReqs(reqs, codeGroups, reason) =>
        ReqsDelete(reqs, codeGroups, reason)

      case UpdateContentCmd.DeleteCodeGroups(ids) =>
        CodeGroupsDelete(ids)

      case UpdateContentCmd.RestoreContent(reqs, reqCodes) =>
        if (reqs.isEmpty && reqCodes.isEmpty)
          fail("No content specified.")
        else
          ContentRestore(reqs, reqCodes)

      case UpdateContentCmd.AddUseCaseStep(ucId, f, at) =>
        val stepId = UseCaseStepId(project.idCeilings.useCaseStep + 1)
        UseCaseStepCreate(stepId, ucId, f, at)

      case UpdateContentCmd.DeleteUseCaseStep(id) =>
        UseCaseStepDelete(id)

      case UpdateContentCmd.RestoreUseCaseStep(id) =>
        UseCaseStepRestore(id)

      case UpdateContentCmd.ShiftUseCaseStepLeft(id) =>
        UseCaseStepShiftLeft(id)

      case UpdateContentCmd.ShiftUseCaseStepRight(id) =>
        UseCaseStepShiftRight(id)
    }

  def updateSavedViews(cmd: SavedViewCmd, project: Project): Result = {
    import shipreq.webapp.member.project.data.savedview._
    cmd match {

      case SavedViewCmd.Create(name, view) =>
        val id = SavedView.Id(project.idCeilings.savedView + 1)
        SavedViewCreate(id, name, view.columns, view.order, view.filterDead, view.filter, view.impGraphConfig)

      case SavedViewCmd.Update(id, vs) =>
        SavedViewUpdate(id, vs)

      case SavedViewCmd.MakeDefault(id) =>
        SavedViewDefaultSet(id)

      case SavedViewCmd.Delete(id) =>
        SavedViewDelete(id)
    }
  }

  def updateManualIssues(cmd: ManualIssueCmd, p: Project): Result =
    cmd match {
      case ManualIssueCmd.Create(txt)     => ManualIssueCreate(p.manualIssues.nextId, txt)
      case ManualIssueCmd.Update(id, txt) => ManualIssueUpdate(id, txt)
      case ManualIssueCmd.Delete(id)      => ManualIssueDelete(id)
    }
}
