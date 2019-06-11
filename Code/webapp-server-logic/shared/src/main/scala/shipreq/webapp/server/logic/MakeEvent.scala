package shipreq.webapp.server.logic

import japgolly.microlibs.nonempty._
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

  def customIssueTypeCrud(a: CustomIssueTypeCrud.RequestType, project: Project): Result =
    a match {

      case CrudAction.Create(vs) =>
        val id = CustomIssueTypeId(project.idCeilings.customIssueType + 1)
        val (key, desc) = vs
        val values = gdAllValues(CustomIssueTypeGD , "")
        CustomIssueTypeCreate(id, values)

      case CrudAction.Update(id, vs) =>
        project.config.customIssueTypes.attempt(id) toMakeEventResult { cur =>
          val (key, desc) = vs
          val vs2 = gdUnequalValues(CustomIssueTypeGD, cur, "")
          eventIfNonEmpty(vs2)(CustomIssueTypeUpdate(id, _))
        }

      case CrudAction.Delete(id) =>
        CustomIssueTypeDelete(id)

      case CrudAction.Restore(id) =>
        CustomIssueTypeRestore(id)
    }

  def customReqTypeCrud(a: CustomReqTypeCrud.RequestType, project: Project): Result =
    a match {

      case CrudAction.Create(vs) =>
        val id = CustomReqTypeId(project.idCeilings.customReqType + 1)
        val (mnemonic, name, imp) = vs
        val values = gdAllValues(CustomReqTypeGD , "")
        CustomReqTypeCreate(id, values)

      case CrudAction.Update(id, vs) =>
        project.config.reqTypes.need(id) match {
          case cur: CustomReqType =>
            val (mnemonic, name, imp) = vs
            val vs2 = gdUnequalValues(CustomReqTypeGD, cur, "")
            eventIfNonEmpty(vs2)(CustomReqTypeUpdate(id, _))
          case f => Failure(s"$f must be a CustomReqType.")
        }

      case CrudAction.Delete(id) =>
        CustomReqTypeDelete(id)

      case CrudAction.Restore(id) =>
        CustomReqTypeRestore(id)
    }

  def fieldCrud(a: FieldMod.RequestType, project: Project): Result = {
    import FieldCrud._
    def nextId = project.idCeilings.customField + 1
    a match {

      case CfgAction.UpdateOrder(id, pos) =>
        FieldReposition(id, pos)

      case CfgAction.Create(vs: TextFieldValues) =>
        val id = CustomField.Text.Id(nextId)
        FieldCustomTextCreate(id, gdAllValues(CustomTextFieldGD, "vs"))

      case CfgAction.Create(vs: TagFieldValues) =>
        val id = CustomField.Tag.Id(nextId)
        FieldCustomTagCreate(id, gdAllValues(CustomTagFieldGD, "vs"))

      case CfgAction.Create(vs: ImplicationFieldValues) =>
        val id = CustomField.Implication.Id(nextId)
        FieldCustomImpCreate(id, gdAllValues(CustomImpFieldGD, "vs"))

      case CfgAction.UpdateValues(id: CustomField.Text.Id, vs: TextFieldValues) =>
        project.config.customFieldAttempt(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomTextFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(FieldCustomTextUpdate(id, _))
        }

      case CfgAction.UpdateValues(id: CustomField.Tag.Id, vs: TagFieldValues) =>
        project.config.customFieldAttempt(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomTagFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(FieldCustomTagUpdate(id, _))
        }

      case CfgAction.UpdateValues(id: CustomField.Implication.Id, vs: ImplicationFieldValues) =>
        project.config.customFieldAttempt(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomImpFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(FieldCustomImpUpdate(id, _))
        }

      case CfgAction.Delete(f: StaticField) =>
        FieldStaticRemove(f)

      case CfgAction.Restore(f: StaticField) =>
        FieldStaticAdd(f)

      case CfgAction.Delete(id: CustomFieldId) =>
        FieldCustomDelete(id)

      case CfgAction.Restore(id: CustomFieldId) =>
        FieldCustomRestore(id)

      case CfgAction.UpdateValues(_: CustomField.Text.Id,        _: TagFieldValues)
         | CfgAction.UpdateValues(_: CustomField.Text.Id,        _: ImplicationFieldValues)
         | CfgAction.UpdateValues(_: CustomField.Tag.Id,         _: TextFieldValues)
         | CfgAction.UpdateValues(_: CustomField.Tag.Id,         _: ImplicationFieldValues)
         | CfgAction.UpdateValues(_: CustomField.Implication.Id, _: TextFieldValues)
         | CfgAction.UpdateValues(_: CustomField.Implication.Id, _: TagFieldValues)
         =>
        Failure(s"Invalid id/value combination: $a")
    }
  }

  // TODO tagCrud protocol is crap. Redo it.
  def tagCrud(a: TagMod.RequestType, project: Project): Result = {
    import TagCrud.{TagGroupValues, ApplicableTagValues}
    def nextId = project.idCeilings.tag + 1
    a match {

      case CrudAction.Create(vs) =>
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

      case CrudAction.Update(tagId, vs) =>
        project.config.tags.tree.get(tagId) match {
          case Some(tit) =>

            var children: Option[TagInTree.Children] = None
            var parents : Option[TagInTree.Parents]  = None
            for (rels <- vs.b) {
              if (tit.children !=* rels.children)
                children = Some(rels.children)
              // TODO Shouldn't need to rebuild treeStructure
              val treeStructure = project.config.tags.tree.mapValues(_.children)
              val ps = MMTree.Relations.deriveParents(tagId, treeStructure)
              if (ps !=* rels.parents)
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

      case CrudAction.Delete(id) =>
        TagDelete(id)

      case CrudAction.Restore(id) =>
        TagRestore(id)
    }
  }

  private def reqCodeIdCounter(project: Project): () => ReqCodeId = {
    var i = project.idCeilings.reqCode
    () => {
      i += 1
      ReqCodeId(i)
    }
  }

  def createContent(cmd: CreateContentCmd, project: Project): Result = {
    val nextCodeId = reqCodeIdCounter(project)
    cmd match {
      case CreateContentCmd.CreateCodeGroup(code, title) =>
        def makeEvent(id: ReqCodeId) =
          Success(CodeGroupCreate(id, gdAllValues(CodeGroupGD, "")))

        project.content.reqCodes.get(code) match {
          case None => makeEvent(nextCodeId())
          case Some(d) =>
            if (d.isActive)
              Failure("Code in use.")
            else
              d.deadGroup match {
                case Some(dg) => makeEvent(dg.id)
                case None     => makeEvent(nextCodeId())
              }
        }

      case i: CreateContentCmd.CreateGenericReq =>
        var vs = GenericReqGD.emptyValues
        for (cs <- NonEmptySet.option(i.codes)) {
          // If a code is in use, ApplyEvent will catch it
          val v = cs.map(c => ReqCode.IdAndValue(nextCodeId(), c))
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
          val v = cs.map(c => ReqCode.IdAndValue(nextCodeId(), c))
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

      case UpdateContentCmd.PatchReqCodes(reqId, cs) => {
        var remove : Set[ReqCodeId]                          = UnivEq.emptySet
        var restore: Set[ReqCodeId]                          = UnivEq.emptySet
        var add    : Multimap[ReqCode.Value, Set, ReqCodeId] = UnivEq.emptySetMultimap
        var r      : Option[Result]                          = None

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
                case None    => add = add.add(c, nextCodeId())
                case Some(i) => restore += i
              }
            }
        }

        r getOrElse ReqCodesPatch(reqId, remove, restore, add)
      }

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
}
