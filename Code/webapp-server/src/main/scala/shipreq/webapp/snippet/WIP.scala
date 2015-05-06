package shipreq.webapp.snippet

import net.liftweb.util.Helpers._
import scalaz.{Equal, \&/, -\/, \/-}, \&/._
import scalaz.syntax.equal._
import shipreq.base.util.{IMap, Util}
import shipreq.base.util.ScalaExt._
import japgolly.nyaya.util._
import shipreq.webapp.base.protocol._
import shipreq.webapp.lib.ServerProtocol
import shipreq.webapp.base.data, data._, DataImplicits._
import shipreq.webapp.base.delta._
import shipreq.webapp.base.text.{Text => T}
import DeletionAction._
import shipreq.webapp.util.QuietException

class WIP {

  // =============================================================================
  // TODO No input should be trusted! Incoming deltas should be validated!
  // =============================================================================

  def newProject = {
    import shipreq.webapp.base.data._
    import shipreq.webapp.base.test.UnsafeTypes._

    val customIssueTypes = RevAnd(10, emptyDataMap(CustomIssueType).addAll(
      CustomIssueType(1, "TO"+"DO", "Something you need To Do.", Alive),
      CustomIssueType(2, "TBD", "To Be Decided.", Alive)))

    val customReqTypes = RevAnd(20, emptyDataMap(CustomReqType).addAll(
      CustomReqType(1, "CO", Set.empty, "Constraint",             ImplicationRequired.Not, Alive),
      CustomReqType(2, "MF", Set.empty, "Major Feature",          ImplicationRequired.Not, Alive),
      CustomReqType(3, "FR", Set.empty, "Functional Requirement", ImplicationRequired,     Alive),
      CustomReqType(4, "BR", Set.empty, "Business Rule",          ImplicationRequired.Not, Alive),
      CustomReqType(5, "DD", Set("DA", "DDF"), "Data Definition", ImplicationRequired.Not, Dead),
      CustomReqType(6, "SI", Set.empty, "Solution Idea",          ImplicationRequired,     Dead)))

    lazy val tagsR = RevAnd(30, tags)
    lazy val v10d = Some("Released: 17/14/1976\nFirst release.")
    lazy val v11d = Some("Released: 1/2/2001")
    lazy val tags = TagTree.empty.addAll(
      TagInTree(TagGroup     (1, "Priority",        None, MutexChildren,     Alive), Vector(2.AT, 3.AT, 4.AT)),
      TagInTree(ApplicableTag(2, "High Priority",   None, "pri=high",        Alive), Vector()),
      TagInTree(ApplicableTag(3, "Medium Priority", None, "pri=med",         Alive), Vector()),
      TagInTree(TagGroup     (10, "Status",         None, MutexChildren.Not, Alive), Vector(11.AT, 12.AT)),
      TagInTree(ApplicableTag(11, "WIP",            None, "wip",             Alive), Vector()),
      TagInTree(ApplicableTag(12, "Deferred",       None, "defer",           Alive), Vector()),
      TagInTree(TagGroup     (20, "Version",        None, MutexChildren.Not, Alive), Vector(27.TG, 21.AT, 25.AT, 26.AT)),
      TagInTree(ApplicableTag(21, "v1.x",           None, "v1.x",            Alive), Vector(22.AT, 23.AT, 24.AT)),
      TagInTree(ApplicableTag(22, "v1.0",           v10d, "v1.0",            Alive), Vector()),
      TagInTree(ApplicableTag(23, "v1.1",           v11d, "v1.1",            Alive), Vector()),
      TagInTree(ApplicableTag(24, "v1.2",           None, "v1.2",            Alive), Vector()),
      TagInTree(ApplicableTag(25, "v2.x",           None, "v2.x",            Alive), Vector()),
      TagInTree(ApplicableTag(26, "v3.x",           None, "v3.x",            Dead ), Vector()),
      TagInTree(TagGroup     (27, "Released",       None, MutexChildren.Not, Alive), Vector(22.AT, 23.AT)),
      TagInTree(ApplicableTag(4, "Low Priority", Some("Nice to have. Stuff that probably won't be implemented."), "pri=low", Alive), Vector()))


    lazy val fields = {
      import CustomField._
      RevAnd(40, FieldSet(emptyDataMap(CustomField).addAll(
        Text       (1, "Description", "desc",     Mandatory,     onlyReqTypes(2, 6, StaticReqType.UseCase), Alive),
        Text       (2, "Notes",       "notes",    Mandatory.Not, notReqTypes(4),                            Alive),
        Text       (3, "Reporter",    "reporter", Mandatory,     onlyReqTypes(5, StaticReqType.UseCase),    Dead),
        Tag        (4, 1.TG,                      Mandatory,     ISubset.All(),                             Alive),
        Tag        (5, 10.TG,                     Mandatory.Not, ISubset.All(),                             Alive),
        Implication(6, 2,                         Mandatory.Not, ISubset.All(),                             Alive)
      ), Vector(
        Text.Id(1), Implication.Id(6), Tag.Id(4), Text.Id(3),
        StaticField.NormalAltStepTree, StaticField.ExceptionStepTree, StaticField.StepGraph,
        Tag.Id(5), Text.Id(2)
      )))
    }

    lazy val reqs     = RevAnd(40, Requirements(IMap.empty(_.id), PubidRegister.empty))
    lazy val reqCodes = RevAnd(50, ReqCodes(Map.empty))
    lazy val reqData  = RevAnd(60, ReqFieldData(Map.empty, Multimap.empty, ReqFieldData.Implications(Multimap.empty)))

    lazy val project = new Project(customIssueTypes, customReqTypes, fields, tagsR, reqs, reqCodes, reqData)

    import shipreq.webapp.base.test.ProjectDSL._

    val List(co,mf,fr) = List[ReqTypeId](1,2,3).map(Some(_))
    val List(p1,p3,p5,rel,wip,v1x) = List[ApplicableTagId](4,3,2,22,11,21)
    val (p2,p4) = (p3,p5)
    val mfs = (0 to 28).toVector.map(i => GenericReqId(i + 1000))

    def fr1Desc = {
      import T.GenericReqTitle._
      Vector(
        EmailAddress("japgolly@gmail.com"), Literal(" is on "), WebAddress("https://github.com"),
        Literal(" cos of "), ReqRef(mfs(6)), Literal(" "), Issue(1, Vector.empty),
        MathTeX("c = \\pm\\sqrt{a^2 + b^2}")
      )
    }
    def fr2Desc = {
      val tbd = {
        import T.InlineIssueDesc._
        Vector(Literal("Pending "), ReqRef(mfs(26)))
      }
      import T.GenericReqTitle._
      Vector(Issue(2, tbd))
    }

    val contentByDsl = (
      GReq(reqType = mf, id = mfs( 1), title = "Use Case Editor"                       , codes = Set("uce")).tag(p5).tag(rel)
    + GReq(reqType = mf, id = mfs( 2), title = "Anonymous Share"                       ).tag(p2).tag(rel)
    + GReq(reqType = mf, id = mfs( 3), title = "Export (PDF, XLS)"                     ).tag(p4)
    + GReq(reqType = mf, id = mfs( 4), title = "Templates"                             ).tag(p2)
    + GReq(reqType = mf, id = mfs( 5), title = "Field Customisation"                   ).tag(p5).tag(wip)
    + GReq(reqType = mf, id = mfs( 6), title = "Incompletions"                         ).tag(p3).tag(wip)
    + GReq(reqType = mf, id = mfs( 7), title = "Organisation"                          ).tag(p5).tag(wip).tag(v1x).tag(rel)
    + GReq(reqType = mf, id = mfs( 8), title = "History/Audit"                         ).tag(p3)
    + GReq(reqType = mf, id = mfs( 9), title = "Collaboration: authoring"              ).tag(p5)
    + GReq(reqType = mf, id = mfs(10), title = "Collaboration: stakeholders"           ).tag(p5)
    + GReq(reqType = mf, id = mfs(11), title = "Collaboration: change mgnt & approval" ).tag(p4)
    + GReq(reqType = mf, id = mfs(12), title = "Low-level Requirements"                ).tag(wip).tag(p5)
    + GReq(reqType = mf, id = mfs(13), title = "Requirement Relationships"             ).tag(wip).tag(p4)
    + GReq(reqType = mf, id = mfs(14), title = "Text-generated Diagrams"               ).tag(p2)
    + GReq(reqType = mf, id = mfs(15), title = "Matrixes"                              ).tag(p2)
    + GReq(reqType = mf, id = mfs(16), title = "CRUDL Matrix"                          ).tag(p1)
    + GReq(reqType = mf, id = mfs(17), title = "Undo & Auto-save"                      ).tag(p2)
    + GReq(reqType = mf, id = mfs(18), title = "Data dictionary"                       ).tag(p1)
    + GReq(reqType = mf, id = mfs(19), title = "Glossary"                              ).tag(p1)
    + GReq(reqType = mf, id = mfs(20), title = "Generic artifact storage"              ).tag(p3)
    + GReq(reqType = mf, id = mfs(21), title = "Doc authoring (V&S, URD, SRS)"         ).tag(p2)
    + GReq(reqType = mf, id = mfs(22), title = "High-level Requirements"               ).tag(p3).tag(wip)
    + GReq(reqType = mf, id = mfs(23), title = "Import external requirements"          ).tag(p2)
    + GReq(reqType = mf, id = mfs(24), title = "Requirement Lint"                      ).tag(p3)
    + GReq(reqType = mf, id = mfs(25), title = "Search"                                ).tag(p2)
    + GReq(reqType = mf, id = mfs(26), title = "Mass text modification (replace)"      ).tag(p1)
    + GReq(reqType = mf, id = mfs(27), title = "External references"                   ).tag(p1)
    + GReq(reqType = mf, id = mfs(28), title = "Entities"                              ).tag(p2)
    + GReq(reqType = fr, title = fr1Desc, codes = Set("uce.sample.1", "uce.sample.1b", "demo.whatever")).impSrc(mfs(12))
    + GReq(reqType = fr, title = fr2Desc, codes = Set("uce.sample.2")).impSrc(mfs(1)).impSrc(mfs(13)).impSrc(mfs(22))
    + RCGroup("demo", Vector(T.ReqCodeGroupTitle.Literal("Demo group header")))
    )

    contentByDsl ! project
  }

  var p = newProject

  val projectInit = ServerProtocol.routine(Routines.ProjectInit)(_ => p)

  // -------------------------------------------------------------------------------------------------------------------
  object reqqq {
    implicit val equality = Equal.equalA[CustomReqType]

    def upd(id: CustomReqTypeId, f: CustomReqType => CustomReqType) =
      mod(_.mod(id, f))

    def modR(f: CustomReqTypeIMap => CustomReqTypeIMap): Option[Rev] = {
      val c = p.customReqTypes
      val a = c.data
      val b = f(a)
      if (a ≟ b)
        None
      else {
        val rev = c.rev.succ
        p = p.copy(customReqTypes = RevAnd(rev, b))
        Some(rev)
      }
    }

    def δ(p: Project) = p.customReqTypes.data.underlyingMap

    def mod(f: CustomReqTypeIMap => CustomReqTypeIMap): RemoteDelta = {
      val p1 = p
      delay()
      modR(f).map(rev => {
        val m1 = δ(p1)
        val m2 = δ(p)
        val delIds = m1.keySet -- m2.keySet
        val updates = m2.toStream.filter{ case (k,v) => !m1.contains(k) || m1(k) != v }.map(_._2).toList
        RemoteDeltaG(Partition.CustomReqTypes, rev, rev)(delIds, updates)
      }).toList
    }

    val crud =
      ServerProtocol.routine(Routines.CustomReqTypeCrud)({
        case CrudAction.Create(v)    =>
          val (mnemonic, name, imp) = v
          val id = CustomReqTypeId(p.customReqTypes.data.keySet.max.value + 1)
          val n = CustomReqType(id, mnemonic, Set.empty, name, imp, Alive)
          mod(_ + n)

        case CrudAction.Update(id, v) =>
          val (mnemonic, name, imp) = v
          upd(id, o => CustomReqType(id, mnemonic, (o.oldMnemonics + o.mnemonic) - mnemonic, name, imp, Alive))

        case CrudAction.Delete(id, HardDel) => mod(_ - id)
        case CrudAction.Delete(id, SoftDel) => upd(id, _.copy(alive = Dead))
        case CrudAction.Delete(id, Restore) => upd(id, _.copy(alive = Alive))
      })

    val imptoggle =
      ServerProtocol.routine(Routines.ReqTypeImplicationMod){
        case (id, imp2) => upd(id, _.copy(imp = imp2))
      }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // TODO Another copy/paste/search/replace
  val issueTypeCrud = {
    implicit val equality = Equal.equalA[CustomIssueType]

    def upd(id: CustomIssueTypeId, f: CustomIssueType => CustomIssueType) =
      mod(_.mod(id, f))

    def modR(f: CustomIssueTypeIMap => CustomIssueTypeIMap): Option[Rev] = {
      val c = p.customIssueTypes
      val a = c.data
      val b = f(a)
      if (a ≟ b)
        None
      else {
        val rev = c.rev.succ
        p = p.copy(customIssueTypes = RevAnd(rev, b))
        Some(rev)
      }
    }

    def δ(p: Project) = p.customIssueTypes.data.underlyingMap

    def mod(f: CustomIssueTypeIMap => CustomIssueTypeIMap): RemoteDelta = {
      val p1 = p
      delay()
      modR(f).map(rev => {
        val m1 = δ(p1)
        val m2 = δ(p)
        val delIds = m1.keySet -- m2.keySet
        val updates = m2.toStream.filter{ case (k,v) => !m1.contains(k) || m1(k) != v }.map(_._2).toList
        RemoteDeltaG(Partition.CustomIssueTypes, rev, rev)(delIds, updates)
      }).toList
    }

    ServerProtocol.routine(Routines.CustomIssueTypeCrud)({
      case CrudAction.Create(v)    =>
        val (key, desc) = v
        val id = CustomIssueTypeId(p.customIssueTypes.data.keySet.max.value + 1)
        val n = CustomIssueType(id, key, desc, Alive)
        mod(_ + n)

      case CrudAction.Update(id, v) =>
        val (key, desc) = v
        upd(id, o => CustomIssueType(id, key, desc, Alive))

      case CrudAction.Delete(id, HardDel) => mod(_ - id)
      case CrudAction.Delete(id, SoftDel) => upd(id, _.copy(alive = Dead))
      case CrudAction.Delete(id, Restore) => upd(id, _.copy(alive = Alive))
    })
  }

  // -------------------------------------------------------------------------------------------------------------------
  object tagCrud {
    import data.{TagId => Id}
    import TagProtocol._

    def modR(f: TagTree => TagTree): Option[Rev] = {
      val rd = p.tags
      val a = rd.data
      val b = f(a)
      if (a ≟ b)
        None
      else {
        val rev = rd.rev.succ
        p = p.copy(tags = RevAnd(rev, b))
        Some(rev)
      }
    }

    def δ(p: Project): Map[Id, PovTag] = {
      val tt = p.tags.data
      val tree = tt.mapValues(_.children)
      tt.mapValues(v => PovTag(v.tag, PovRelations.derive(v.tag.id, tree)))
    }

    // TODO Another copy/paste/search/replace
    def mod(f: TagTree => TagTree): RemoteDelta = {
      val p1 = p
      delay()
      modR(f).map(rev => {
        val m1 = δ(p1)
        val m2 = δ(p)
        val delIds = m1.keySet -- m2.keySet
        val updates = m2.toStream.filter{ case (k,v) => !m1.contains(k) || m1(k) != v }.map(_._2).toList
        RemoteDeltaG(Partition.Tags, rev, rev)(delIds, updates)
      }).toList
    }

    def upd(id: Id, f: Tag => Tag): RemoteDelta =
      mod(updT(id, f))

    def updT(id: Id, f: Tag => Tag): TagTree => TagTree =
      _.mod(id, v => v.copy(tag = f(v.tag)))

    def put(i: Id, v: Values \&/ PovRelations): RemoteDelta = v match {
      case This(a)    => put2(i, a.some, None)
      case That(b)    => put2(i, None  , b.some)
      case Both(a, b) => put2(i, a.some, b.some)
    }

    // TODO Crud routines don't allow validation?? That's a massive oversight.

    def put2(i: Id, ov: Option[Values], or: Option[PovRelations]): RemoteDelta =
      mod(tt => {
        val res = ov.fold(tt) { v =>
          val newTag = build(i)(v)
          tt.modOrPut(i, _.copy(tag = newTag), TagInTree(newTag, Vector.empty))
        }
        or.fold(res)(PovRelations.trustedApply1(_, i, res)) // TODO Possible cycle error
      })

    def nextId: TagId = TagGroupId(p.tags.data.keySet.map(_.value).max + 1)
    implicit def genIdToTG(g: TagId) = TagGroupId(g.value)
    implicit def genIdToAT(g: TagId) = ApplicableTagId(g.value)

    def build(i: Id): Values => Tag = {
      case TagGroupValues(n, mc, d)     => TagGroup(i, n, d, mc, Alive)
      case ApplicableTagValues(n, k, d) => ApplicableTag(i, n, d, k, Alive)
    }

    def setLife(t0: TagTree, id: Id, oa: Option[Alive]): TagTree =
      t0.get(id).fold(t0){ subj =>

        // Modify children
        val t1 = subj.children.foldLeft(t0) { (t, childId) =>
          t.get(childId).map(_.tag) match {
            case Some(child) if child.alive ≟ subj.tag.alive =>
              val childToParents = Multimap(t.mapValues(_.children.toSet)).reverse // TODO I *HATE* performance!
              val hasLiveParent = (childToParents(childId) - id).exists(p => t.underlyingMap(p).tag.alive ≟ Alive)
              if (hasLiveParent)
                t
              else
                setLife(t, childId, oa)
            case _ => t
          }
        }

        // Modify subject
        oa match {
          case None    => t1.mapUnderlying(_.mapValues(_ removeChild id) - id) // copy from RemoteDelta
          case Some(a) => updT(id, Tag.alive set a)(t1)
        }
      }

    val fn = ServerProtocol.routine(Routines.TagCrud)({
        case CrudAction.Create(v)           => put(nextId, v)
        case CrudAction.Update(i, v)        => put(i, v)
        case CrudAction.Delete(id, HardDel) => mod(setLife(_, id, None))
        case CrudAction.Delete(id, SoftDel) => mod(setLife(_, id, Some(Dead)))
        case CrudAction.Delete(id, Restore) => mod(setLife(_, id, Some(Alive)))
      })
  }

  // -------------------------------------------------------------------------------------------------------------------
  object fieldCrud {
    import FieldProtocol._
    import CfgAction._
    import shipreq.webapp.base.data.{CustomField => CF}

    def apply(deletions: Set[Field.Id], updates: List[Delta]): RemoteDelta = {
      delay()
      val rev = p.fields.rev.succ
      val rdg = RemoteDeltaG(Partition.Fields, rev, rev)(deletions, updates)
      val p2 = PartitionFns.update(p, rev, rdg.forceDeltaP(Partition.Fields))
      if (p.fields.data ≟ p2.fields.data)
        Nil
      else {
        p = p2
        List(rdg)
      }
    }

    def mod(f: FieldSet => List[Delta]): RemoteDelta =
      apply(Set.empty, f(p.fields.data))

    def mod(id: CF.Id)(f: CF => CF): RemoteDelta =
      mod(fs => fs.customFields.get(id).fold(∅)(newField =>
        List(Delta(\/-(f(newField)), Util.position(fs.order, id)))))

    @inline def ∅ = List.empty[Delta]

    def nextId(fs: FieldSet): CF.Id = CF.Text.Id(fs.customFields.keys.max.value + 1)
    implicit def genIdToText(g: CF.Id) = CF.Text.Id(g.value)
    implicit def genIdToTag (g: CF.Id) = CF.Tag.Id(g.value)
    implicit def genIdToImpl(g: CF.Id) = CF.Implication.Id(g.value)

    val cfgAction =
      ServerProtocol.routine(Routines.FieldCrud){

        case Create(TextFieldValues(n, k, m, r)) =>
          mod { fs =>
            val f = CF.Text(nextId(fs), n, k, m, r, Alive)
            List(Delta(\/-(f), None))
          }

        case Create(TagFieldValues(t, m, r)) =>
          mod { fs =>
            val f = CF.Tag(nextId(fs), t, m, r, Alive)
            List(Delta(\/-(f), None))
          }

        case Create(ImplicationFieldValues(t, m, r)) =>
          mod { fs =>
            val f = CF.Implication(nextId(fs), t, m, r, Alive)
            List(Delta(\/-(f), None))
          }

        case UpdateValues(id, v) =>
          mod(id)(cf => (cf, v) match {
            case (CF.Text       (_, _, _, _, _, Alive), TextFieldValues       (n, k, m, r)) => CF.Text       (id, n, k, m, r, Alive)
            case (CF.Tag        (_,    _, _, _, Alive), TagFieldValues        (t,    m, r)) => CF.Tag        (id, t,    m, r, Alive)
            case (CF.Implication(_,    _, _, _, Alive), ImplicationFieldValues(t,    m, r)) => CF.Implication(id, t,    m, r, Alive)
            case _ => cf
          })

        case UpdateOrder(f: StaticField, p) =>
          mod(fs => List(Delta(-\/(f), p)))

        case UpdateOrder(id: CF.Id, p) =>
          mod(_.customFields.get(id).fold(∅)(f => List(Delta(\/-(f), p))))

        case Delete(f: StaticField, Restore) =>
          mod(fs => if (fs.order contains f) Nil else List(Delta(-\/(f), None)))

        case Delete(id: CF.Id, Restore) =>
          mod(id)(CF.alive set Alive)

        case Delete(f: StaticField, HardDel | SoftDel) =>
          f.deletable match {
            case Deletable     => apply(Set(f), Nil)
            case Deletable.Not => Nil
          }

        case Delete(id: CF.Id, SoftDel) =>
          mod(id)(CF.alive set Dead)

        case Delete(id: CF.Id, HardDel) =>
          apply(Set(id), Nil)
      }

    val mandmod =
      ServerProtocol.routine(Routines.FieldMandatorinessMod){
        case (id, m) => mod(id)(CF.mandatory set m)
      }

  }

  // -------------------------------------------------------------------------------------------------------------------

  def delay(): Unit = Thread.sleep(new java.util.Random().nextInt(80)+80)

  def render = {
    val pg = Routines.ProjectSPA(
      projectInit,
      issueTypeCrud,
      reqqq.crud, reqqq.imptoggle,
      fieldCrud.mandmod, fieldCrud.cfgAction,
      tagCrud.fn)
    val js = ServerProtocol.invokeClientHtml(JsEntryPoint.reactExamples)(pg)
    "*" #> js
  }
}
