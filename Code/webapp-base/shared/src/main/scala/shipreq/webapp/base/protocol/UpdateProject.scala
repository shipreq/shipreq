package shipreq.webapp.base.protocol

import scalaz.\/
import scalaz.syntax.equal._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash.HashScheme
import shipreq.webapp.base.util.GenericDataMacros._
import DataImplicits._
import UnivEq.{string, option, vector, map}

object UpdateProject {

  // ALWAYS use the latest to ensure that all parts of Project are hashed.
  // Alternative hash schemes exist so that Project can evolve without breaking old hashes.
  // New events should NEVER use old hash schemes.
  val hashScheme = HashScheme.latest
  val hashProject = hashScheme.hashProject

  case class State(hash: Int, project: Project)

  sealed trait MakeEventResult
  sealed trait UpdateResult

  case class  MadeEvent(e: Event)                        extends                   MakeEventResult
  case object NoChange                                   extends UpdateResult with MakeEventResult
  case class  Failed(reason: String)                     extends UpdateResult with MakeEventResult
  case class  Updated(state: State, ves: VerifiedEvents) extends UpdateResult

  // ===================================================================================================================

  @inline private implicit def autoVectorVE(ve: VerifiedEvent): VerifiedEvents = Vector1(ve)

  //  @inline def must[A, R >: Failed](m: Must[A])(f: A => R): R =
  //    m.fold(Failed, f)

  @inline private implicit class MustExt[A](private val m: Must[A]) extends AnyVal {
    @inline def toMakeEventResult(f: A => MakeEventResult): MakeEventResult = m.fold(Failed, f)
  }

  @inline private implicit class DisjunctionExt[A](private val d: String \/ A) extends AnyVal {
    @inline def toUpdateResult(f: A => UpdateResult): UpdateResult = d.fold(Failed, f)
  }

  def eventIfNonEmpty[A](a: A)(f: NonEmpty[A] => Event)(implicit proof: NonEmpty.ProofA[A]): MakeEventResult =
    NonEmpty.tryO(a) match {
      case Some(b) => MadeEvent(f(b))
      case None    => NoChange
    }

  def applyEvent(e: Event, state: State): UpdateResult =
    ApplyEvent.untrusted.apply1(e)(state.project) toUpdateResult { p2 =>
      val h2 = hashProject.hash(p2)
      if (h2 == state.hash)
        NoChange
      else {
        val s2 = State(h2, p2)
        val ve = VerifiedEvent(hashScheme, h2, e)
        Updated(s2, ve)
      }
    }

  def applyEventR(state: State)(r: MakeEventResult): UpdateResult =
    r match {
      case MadeEvent(e)    => applyEvent(e, state)
      case u: UpdateResult => u
    }

  @inline private implicit def autoMadeEvent(e: Event) = MadeEvent(e)

  // ===================================================================================================================

  def initState(p: Project): State =
    State(hashProject hash p, p)

  def reqTypeImplicationMod(input: RemoteFns.ReqTypeImplicationMod.Input, state: State): UpdateResult = {
    val (id, imp) = input
    val e = UpdateCustomReqType(id, CustomReqTypeGD.Imp(imp))
    applyEvent(e, state)
  }

  def fieldMandatorinessMod(input: RemoteFns.FieldMandatorinessMod.Input, state: State): UpdateResult = {
    val m = input._2
    val e = input._1 match {
      case id: CustomField.Text       .Id => UpdateCustomTextField(id, CustomTextFieldGD.Mandatory(m))
      case id: CustomField.Tag        .Id => UpdateCustomTagField (id, CustomTagFieldGD .Mandatory(m))
      case id: CustomField.Implication.Id => UpdateCustomImpField (id, CustomImpFieldGD .Mandatory(m))
    }
    applyEvent(e, state)
  }

  def customIssueTypeCrud(a: RemoteFns.CustomIssueTypeCrud.Action, state: State): UpdateResult = {
    applyEventR(state)(a match {

      case CrudAction.Create(vs) =>
        val id = CustomIssueTypeId(state.project.idCeilings.customIssueType + 1)
        val (key, desc) = vs
        val values = gdAllValues(CustomIssueTypeGD , "")
        CreateCustomIssueType(id, values)

      case CrudAction.Update(id, vs) =>
        state.project.config.customIssueType(id) toMakeEventResult { cur =>
          val (key, desc) = vs
          val vs2 = gdUnequalValues(CustomIssueTypeGD, cur, "")
          eventIfNonEmpty(vs2)(UpdateCustomIssueType(id, _))
        }

      case CrudAction.Delete(id, da) =>
        DeleteCustomIssueType(id, da)
    })
  }

  def customReqTypeCrud(a: RemoteFns.CustomReqTypeCrud.Action, state: State): UpdateResult = {
    applyEventR(state)(a match {

      case CrudAction.Create(vs) =>
        val id = CustomReqTypeId(state.project.idCeilings.customReqType + 1)
        val (mnemonic, name, imp) = vs
        val values = gdAllValues(CustomReqTypeGD , "")
        CreateCustomReqType(id, values)

      case CrudAction.Update(id, vs) =>
        state.project.config.reqTypeC(id) toMakeEventResult { cur =>
          val (mnemonic, name, imp) = vs
          val vs2 = gdUnequalValues(CustomReqTypeGD, cur, "")
          eventIfNonEmpty(vs2)(UpdateCustomReqType(id, _))
        }

      case CrudAction.Delete(id, da) =>
        DeleteCustomReqType(id, da)
    })
  }

  def fieldCrud(a: RemoteFns.FieldCrud.Input, state: State): UpdateResult = {
    import FieldProtocol._
    def nextId = state.project.idCeilings.customField + 1
    applyEventR(state)(a match {

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
        state.project.config.customField(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomTextFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(UpdateCustomTextField(id, _))
        }

      case CfgAction.UpdateValues(id: CustomField.Tag.Id, vs: TagFieldValues) =>
        state.project.config.customField(id) toMakeEventResult { cur =>
          val vs2 = gdUnequalValues(CustomTagFieldGD, cur, "vs")
          eventIfNonEmpty(vs2)(UpdateCustomTagField(id, _))
        }

      case CfgAction.UpdateValues(id: CustomField.Implication.Id, vs: ImplicationFieldValues) =>
        state.project.config.customField(id) toMakeEventResult { cur =>
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
    })
  }

  // TODO tagCrud protocol is crap. Redo it.
  def tagCrud(a: RemoteFns.TagCrud.Action, state: State): UpdateResult = {
    import TagProtocol.{TagGroupValues, ApplicableTagValues}
    def nextId = state.project.idCeilings.tag + 1
    applyEventR(state)(a match {

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
        state.project.config.tags.data.get(tagId) match {
          case Some(tit) =>

            var children: Option[TagInTree.Children] = None
            var parents : Option[TagInTree.Parents]  = None
            for (rels <- vs.b) {
              if (tit.children ≠ rels.children)
                children = Some(rels.children)
              // TODO Shouldn't need to rebuild treeStructure
              val treeStructure = state.project.config.tags.data.mapValues(_.children)
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
    })
  }

}
