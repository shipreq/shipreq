package shipreq.webapp.server.logic

import japgolly.microlibs.nonempty._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import nyaya.util.Multimap
import scalaz.\/
import scalaz.syntax.equal._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.ProjectSpaProtocols.WsReqRes.{ProjectNameSet => _, _}
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.util.GenericDataMacros._
import DataImplicits._
import Event._
import ScalaExt._
import PotentialChange._

/**
 * Translates commands inputs into [[ActiveEvent]]s.
 */
object MakeEvent {

  type Result = PotentialChange[String, ActiveEvent]

  // ===================================================================================================================

  @inline private implicit class DisjExt[A](private val v: String \/ A) extends AnyVal {
    @inline def toMakeEventResult(f: A => Result): Result =
      v.fold(Failure(_), f)
  }

  private def eventIfNonEmpty[A](a: A)(f: NonEmpty[A] => Result)(implicit proof: NonEmpty.ProofMono[A]): Result =
    NonEmpty(a) match {
      case Some(b) => f(b)
      case None    => Unchanged
    }

  @inline private implicit def autoSuccess(e: ActiveEvent) = Success(e)

  // ===================================================================================================================

  def projectNameSetFn(name: String): Result =
    ProjectNameSet(name)

  def reqTypeImplicationMod(input: ReqTypeImplicationMod.RequestType): Result = {
    val (id, imp) = input
    CustomReqTypeUpdate(id, CustomReqTypeGD.Imp(imp))
  }

  def fieldMandatorinessMod(input: FieldMandatorinessMod.RequestType): Result = {
    val m = input._2
    input._1 match {
      case id: CustomField.Text       .Id => FieldCustomTextUpdate(id, CustomTextFieldGD.Mandatory(m))
      case id: CustomField.Tag        .Id => FieldCustomTagUpdate (id, CustomTagFieldGD .Mandatory(m))
      case id: CustomField.Implication.Id => FieldCustomImpUpdate (id, CustomImpFieldGD .Mandatory(m))
    }
  }

  def updateConfig(cmd: UpdateConfigCmd, project: Project): Result = {
    def nextId = project.idCeilings.customField + 1

    cmd match {

      case UpdateConfigCmd.CustomIssueTypeCreate(vs) =>
        val id = CustomIssueTypeId(project.idCeilings.customIssueType + 1)
        import vs._
        val values = gdAllValues(CustomIssueTypeGD , "")
        CustomIssueTypeCreate(id, values)

      case UpdateConfigCmd.CustomIssueTypeUpdate(id, vs) =>
        project.config.customIssueTypes.attempt(id) toMakeEventResult { cur =>
          import vs._
          val vs2 = gdUnequalValues(CustomIssueTypeGD, cur, "")
          eventIfNonEmpty(vs2)(CustomIssueTypeUpdate(id, _))
        }

      case UpdateConfigCmd.CustomIssueTypeDelete(id) =>
        CustomIssueTypeDelete(id)

      case UpdateConfigCmd.CustomIssueTypeRestore(id) =>
        CustomIssueTypeRestore(id)

      case UpdateConfigCmd.CustomReqTypeCreate(vs) =>
        val id = CustomReqTypeId(project.idCeilings.customReqType + 1)
        import vs._
        val values = gdAllValues(CustomReqTypeGD , "")
        CustomReqTypeCreate(id, values)

      case UpdateConfigCmd.CustomReqTypeUpdate(id, vs) =>
        project.config.reqTypes.need(id) match {
          case cur: CustomReqType =>
            import vs._
            val vs2 = gdUnequalValues(CustomReqTypeGD, cur, "")
            eventIfNonEmpty(vs2)(CustomReqTypeUpdate(id, _))
          case f => Failure(s"$f must be a CustomReqType.")
        }

      case UpdateConfigCmd.CustomReqTypeDelete(id) =>
        CustomReqTypeDelete(id)

      case UpdateConfigCmd.CustomReqTypeRestore(id) =>
        CustomReqTypeRestore(id)

      case UpdateConfigCmd.FieldUpdateOrder(id, pos) =>
        FieldReposition(id, pos)

      case UpdateConfigCmd.CustomFieldCreate(vs: UpdateConfigCmd.TextFieldValues) =>
        val _ = vs // used by macros
        val id = CustomField.Text.Id(nextId)
        FieldCustomTextCreate(id, gdAllValues(CustomTextFieldGD, "vs"))

      case UpdateConfigCmd.CustomFieldCreate(vs: UpdateConfigCmd.TagFieldValues) =>
        val _ = vs // used by macros
        val id = CustomField.Tag.Id(nextId)
        FieldCustomTagCreate(id, gdAllValues(CustomTagFieldGD, "vs"))

      case UpdateConfigCmd.CustomFieldCreate(vs: UpdateConfigCmd.ImpFieldValues) =>
        val _ = vs // used by macros
        val id = CustomField.Implication.Id(nextId)
        FieldCustomImpCreate(id, gdAllValues(CustomImpFieldGD, "vs"))

      case UpdateConfigCmd.CustomFieldUpdateText(id, vs) =>
        val _ = vs // used by macros
        project.config.fields.customAttempt(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomTextFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(FieldCustomTextUpdate(id, _))
        }

      case UpdateConfigCmd.CustomFieldUpdateTag(id, vs) =>
        val _ = vs // used by macros
        project.config.fields.customAttempt(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomTagFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(FieldCustomTagUpdate(id, _))
        }

      case UpdateConfigCmd.CustomFieldUpdateImp(id, vs) =>
        val _ = vs // used by macros
        project.config.fields.customAttempt(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomImpFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(FieldCustomImpUpdate(id, _))
        }

      case UpdateConfigCmd.FieldDelete(f: StaticField) =>
        FieldStaticRemove(f)

      case UpdateConfigCmd.FieldRestore(f: StaticField) =>
        FieldStaticAdd(f)

      case UpdateConfigCmd.FieldDelete(id: CustomFieldId) =>
        FieldCustomDelete(id)

      case UpdateConfigCmd.FieldRestore(id: CustomFieldId) =>
        FieldCustomRestore(id)

      case t: UpdateConfigCmd.ToModifyTags =>
        tagCrud(t, project)
    }
  }

  // TODO tagCrud protocol is crap. Redo it.
  private def tagCrud(cmd: UpdateConfigCmd.ToModifyTags, project: Project): Result = {
    import UpdateConfigCmd.{TagGroupValues, ApplicableTagValues}
    def nextId = project.idCeilings.tag + 1
    cmd match {

      case UpdateConfigCmd.TagCreate(vs) =>
        val rels = vs.b getOrElse TagInTree.noRelations
        import rels._

        vs.a match {
          case Some(v: ApplicableTagValues) =>
            val id = ApplicableTagId(nextId)
            import v._
            ApplicableTagCreate(id, gdAllValues(ApplicableTagGD, ""))

          case Some(v: TagGroupValues) =>
            val id = TagGroupId(nextId)
            import v._
            TagGroupCreate(id, gdAllValues(TagGroupGD, ""))

          case None => Failure("Values required.")
        }

      case UpdateConfigCmd.TagSetApplicableChildrenOrder(tagId, childrenA) =>
        val existingChildrenA = project.config.tags.directChildren(tagId).iterator.filterSubType[ApplicableTagId].toSet
        if (existingChildrenA !=* childrenA.toSet)
          Failure("Tag group contains different children than specified. Please try again.")
        else {
          val childrenG = project.config.tags.directTagGroupChildren(tagId)
          val children: Vector[TagId] = childrenG ++ childrenA
          val values = TagGroupGD.nev(TagGroupGD.ValueForChildren(children))
          TagGroupUpdate(tagId, values)
        }

      case UpdateConfigCmd.TagUpdate(tagId, vs) =>
        project.config.tags.tree.get(tagId) match {
          case Some(tit) =>

            var children: Option[TagInTree.Children] = None
            var parents : Option[TagInTree.Parents]  = None
            for (rels <- vs.b) {
              if (tit.children !=* rels.children)
                children = Some(rels.children)
              val existingParents = project.config.tags.parents(tagId)
              if (existingParents !=* rels.parents)
                parents = Some(rels.parents)
            }

            tit.tag match {
              case cur: ApplicableTag =>
                import ApplicableTagGD._
                var us = emptyValues
                def build = eventIfNonEmpty(us)(ApplicableTagUpdate(cur.id, _))
                children.foreach(c => us += Children(c))
                parents .foreach(p => us += Parents (p))
                vs.a match {
                  case Some(v: ApplicableTagValues) =>
                    if (v.name !=* cur.name) us += Name(v.name)
                    if (v.key  !=* cur.key ) us += Key (v.key)
                    if (v.desc !=* cur.desc) us += Desc(v.desc)
                    build
                  case None =>
                    build
                  case Some(_: TagGroupValues) =>
                    Failure("Cannot apply TagGroup values to an ApplicableTag.")
                }

              case cur: TagGroup =>
                import TagGroupGD._
                var us = emptyValues
                def build = eventIfNonEmpty(us)(TagGroupUpdate(cur.id, _))
                children.foreach(c => us += Children(c))
                parents .foreach(p => us += Parents (p))
                vs.a match {
                  case Some(v: TagGroupValues) =>
                    if (v.name          !=* cur.name         ) us += Name         (v.name)
                    if (v.mutexChildren !=* cur.mutexChildren) us += MutexChildren(v.mutexChildren)
                    if (v.desc          !=* cur.desc         ) us += Desc         (v.desc)
                    build
                  case None =>
                    build
                  case Some(_: ApplicableTagValues) =>
                    Failure("Cannot apply ApplicableTag values to an TagGroup.")
                }

            }
          case None => Failure(s"$tagId not found.")
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
        val _ = title // used by macros

        def makeEvent(id: ReqCodeGroupId) =
          Success(CodeGroupCreate(id, gdAllValues(CodeGroupGD, "")))

        project.content.reqCodes.get(code) match {
          case None => makeEvent(nextCodeId.group())
          case Some(d) =>
            if (d.isActive)
              Failure("Code in use.")
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
        for (v <- NonEmptyVector.option(i.title))        vs += GenericReqGD.Title(v)
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
        for (v <- NonEmptyVector.option(i.title))        vs += UseCaseGD.Title(v)
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
          r = Some(Failure(err))

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
          Failure("No content specified.")
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
    import reqtable._
    cmd match {

      case SavedViewCmd.Create(name, view) =>
        val id = SavedView.Id(project.idCeilings.reqtableView + 1)
        SavedViewCreate(id, name, view.columns, view.order, view.filterDead, view.filter)

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
