package shipreq.webapp.server.protocol

import scalaz.syntax.equal._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.util.GenericDataMacros._
import DataImplicits._
import UnivEq.Implicits._

/**
 * Translates [[RemoteFn]] inputs into [[ActiveEvent]]s.
 */
object MakeEvent {

  sealed trait Result
  case class  MadeEvent(e: ActiveEvent) extends Result
  case class  Failed(reason: String)    extends Result
  case object NoChange                  extends Result

  // ===================================================================================================================

  @inline private implicit class MustExt[A](private val m: Must[A]) extends AnyVal {
    @inline def toMakeEventResult(f: A => Result): Result =
      m.fold(Failed, f)
  }

  private def eventIfNonEmpty[A](a: A)(f: NonEmpty[A] => ActiveEvent)(implicit proof: NonEmpty.ProofA[A]): Result =
    NonEmpty.tryO(a) match {
      case Some(b) => MadeEvent(f(b))
      case None    => NoChange
    }

  @inline private implicit def autoMadeEvent(e: ActiveEvent) = MadeEvent(e)

  // ===================================================================================================================

  def reqTypeImplicationMod(input: ReqTypeImplicationMod.Input): Result = {
    val (id, imp) = input
    UpdateCustomReqType(id, CustomReqTypeGD.Imp(imp))
  }

  def fieldMandatorinessMod(input: FieldMandatorinessMod.Input): Result = {
    val m = input._2
    input._1 match {
      case id: CustomField.Text       .Id => UpdateCustomTextField(id, CustomTextFieldGD.Mandatory(m))
      case id: CustomField.Tag        .Id => UpdateCustomTagField (id, CustomTagFieldGD .Mandatory(m))
      case id: CustomField.Implication.Id => UpdateCustomImpField (id, CustomImpFieldGD .Mandatory(m))
    }
  }

  def customIssueTypeCrud(a: CustomIssueTypeCrud.Action, project: Project): Result =
    a match {

      case CrudAction.Create(vs) =>
        val id = CustomIssueTypeId(project.idCeilings.customIssueType + 1)
        val (key, desc) = vs
        val values = gdAllValues(CustomIssueTypeGD , "")
        CreateCustomIssueType(id, values)

      case CrudAction.Update(id, vs) =>
        project.config.customIssueType(id) toMakeEventResult { cur =>
          val (key, desc) = vs
          val vs2 = gdUnequalValues(CustomIssueTypeGD, cur, "")
          eventIfNonEmpty(vs2)(UpdateCustomIssueType(id, _))
        }

      case CrudAction.Delete(id, da) =>
        DeleteCustomIssueType(id, da)
    }

  def customReqTypeCrud(a: CustomReqTypeCrud.Action, project: Project): Result =
    a match {

      case CrudAction.Create(vs) =>
        val id = CustomReqTypeId(project.idCeilings.customReqType + 1)
        val (mnemonic, name, imp) = vs
        val values = gdAllValues(CustomReqTypeGD , "")
        CreateCustomReqType(id, values)

      case CrudAction.Update(id, vs) =>
        project.config.reqTypeC(id) toMakeEventResult { cur =>
          val (mnemonic, name, imp) = vs
          val vs2 = gdUnequalValues(CustomReqTypeGD, cur, "")
          eventIfNonEmpty(vs2)(UpdateCustomReqType(id, _))
        }

      case CrudAction.Delete(id, da) =>
        DeleteCustomReqType(id, da)
    }

  def fieldCrud(a: FieldCrud.Fn.Input, project: Project): Result = {
    import FieldCrud._
    def nextId = project.idCeilings.customField + 1
    a match {

      case CfgAction.UpdateOrder(id, pos) =>
        RepositionField(id, pos)

      case CfgAction.Create(vs: TextFieldValues) =>
        val id = CustomField.Text.Id(nextId)
        CreateCustomTextField(id, gdAllValues(CustomTextFieldGD, "vs"))

      case CfgAction.Create(vs: TagFieldValues) =>
        val id = CustomField.Tag.Id(nextId)
        CreateCustomTagField(id, gdAllValues(CustomTagFieldGD, "vs"))

      case CfgAction.Create(vs: ImplicationFieldValues) =>
        val id = CustomField.Implication.Id(nextId)
        CreateCustomImpField(id, gdAllValues(CustomImpFieldGD, "vs"))

      case CfgAction.UpdateValues(id: CustomField.Text.Id, vs: TextFieldValues) =>
        project.config.customField(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomTextFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(UpdateCustomTextField(id, _))
        }

      case CfgAction.UpdateValues(id: CustomField.Tag.Id, vs: TagFieldValues) =>
        project.config.customField(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomTagFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(UpdateCustomTagField(id, _))
        }

      case CfgAction.UpdateValues(id: CustomField.Implication.Id, vs: ImplicationFieldValues) =>
        project.config.customField(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomImpFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(UpdateCustomImpField(id, _))
        }

      case CfgAction.Delete(f: StaticField, Restore) =>
        AddStaticField(f)

      case CfgAction.Delete(f: StaticField, HardDel | SoftDel) =>
        DeleteStaticField(f)

      case CfgAction.Delete(id: CustomFieldId, da) =>
        DeleteCustomField(id, da)

      case CfgAction.UpdateValues(_: CustomField.Text.Id,        _: TagFieldValues)
         | CfgAction.UpdateValues(_: CustomField.Text.Id,        _: ImplicationFieldValues)
         | CfgAction.UpdateValues(_: CustomField.Tag.Id,         _: TextFieldValues)
         | CfgAction.UpdateValues(_: CustomField.Tag.Id,         _: ImplicationFieldValues)
         | CfgAction.UpdateValues(_: CustomField.Implication.Id, _: TextFieldValues)
         | CfgAction.UpdateValues(_: CustomField.Implication.Id, _: TagFieldValues)
         =>
        Failed(s"Invalid id/value combination: $a")
    }
  }

  // TODO tagCrud protocol is crap. Redo it.
  def tagCrud(a: TagCrud.Fn.Action, project: Project): Result = {
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
            CreateApplicableTag(id, gdAllValues(ApplicableTagGD, ""))

          case Some(v: TagGroupValues) =>
            val id = TagGroupId(nextId)
            import v._
            CreateTagGroup(id, gdAllValues(TagGroupGD, ""))

          case None => Failed("Values required.")
        }

      case CrudAction.Update(tagId, vs) =>
        project.config.tags.get(tagId) match {
          case Some(tit) =>

            var children: Option[TagInTree.Children] = None
            var parents : Option[TagInTree.Parents]  = None
            for (rels <- vs.b) {
              if (tit.children ≠ rels.children)
                children = Some(rels.children)
              // TODO Shouldn't need to rebuild treeStructure
              val treeStructure = project.config.tags.mapValues(_.children)
              val ps = MMTree.Relations.deriveParents(tagId, treeStructure)
              if (ps ≠ rels.parents)
                parents = Some(rels.parents)
            }

            tit.tag match {
              case cur: ApplicableTag =>
                import ApplicableTagGD._
                var us = emptyValues
                def build = eventIfNonEmpty(us)(UpdateApplicableTag(cur.id, _))
                children.foreach(c => us += Children(c))
                parents .foreach(p => us += Parents (p))
                vs.a match {
                  case Some(v: ApplicableTagValues) =>
                    if (v.name ≠ cur.name) us += Name(v.name)
                    if (v.key  ≠ cur.key ) us += Key (v.key)
                    if (v.desc ≠ cur.desc) us += Desc(v.desc)
                    build
                  case None =>
                    build
                  case Some(_: TagGroupValues) =>
                    Failed("Cannot apply TagGroup values to an ApplicableTag.")
                }

              case cur: TagGroup =>
                import TagGroupGD._
                var us = emptyValues
                def build = eventIfNonEmpty(us)(UpdateTagGroup(cur.id, _))
                children.foreach(c => us += Children(c))
                parents .foreach(p => us += Parents (p))
                vs.a match {
                  case Some(v: TagGroupValues) =>
                    if (v.name          ≠ cur.name         ) us += Name         (v.name)
                    if (v.mutexChildren ≠ cur.mutexChildren) us += MutexChildren(v.mutexChildren)
                    if (v.desc          ≠ cur.desc         ) us += Desc         (v.desc)
                    build
                  case None =>
                    build
                  case Some(_: ApplicableTagValues) =>
                    Failed("Cannot apply ApplicableTag values to an TagGroup.")
                }

            }
          case None => Failed(s"$tagId not found.")
        }

      case CrudAction.Delete(id, da) =>
        DeleteTag(id, da)
    }
  }

}
