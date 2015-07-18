package shipreq.webapp.snippet

import net.liftweb.util.Helpers._
import scalaz.{Equal, \/, \&/, -\/, \/-}, \&/._
import scalaz.syntax.equal._
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import japgolly.nyaya.util.{Util => _, _}
import shipreq.webapp.base.protocol._
import shipreq.webapp.lib.ServerProtocol
import shipreq.webapp.base.data, data._, DataImplicits._
import shipreq.webapp.base.delta._
import shipreq.webapp.base.event.{DeletionAction, HardDel, SoftDel, Restore}
import shipreq.webapp.base.text.{Text => T}
import shipreq.webapp.util.QuietException

class WIP {

  // =============================================================================
  // No input should be trusted! Incoming deltas should be validated!
  // Needless updates should be avoided.
  // =============================================================================

  def newProject = {
    import shipreq.webapp.base.data._
    import shipreq.webapp.base.test.UnsafeTypes._

    val customIssueTypes = RevAnd(10, emptyDataMap(CustomIssueType).addAll(
      CustomIssueType(1, "TO"+"DO", "Something you need To Do.", Live),
      CustomIssueType(2, "TBD", "To Be Decided.", Live)))

    val customReqTypes = RevAnd(20, emptyDataMap(CustomReqType).addAll(
      CustomReqType(1, "CO", Set.empty, "Constraint",             ImplicationRequired.Not, Live),
      CustomReqType(2, "MF", Set.empty, "Major Feature",          ImplicationRequired.Not, Live),
      CustomReqType(3, "FR", Set.empty, "Functional Requirement", ImplicationRequired,     Live),
      CustomReqType(4, "BR", Set.empty, "Business Rule",          ImplicationRequired.Not, Live),
      CustomReqType(5, "DD", Set("DA", "DDF"), "Data Definition", ImplicationRequired.Not, Dead),
      CustomReqType(6, "SI", Set.empty, "Solution Idea",          ImplicationRequired,     Dead)))

    lazy val tagsR = RevAnd(30, tags)
    lazy val v10d = Some("Released: 17/14/1976\nFirst release.")
    lazy val v11d = Some("Released: 1/2/2001")
    lazy val tags = TagTree.empty.addAll(
      TagInTree(TagGroup     (1, "Priority",        None, MutexChildren,     Live), Vector(2.AT, 3.AT, 4.AT)),
      TagInTree(ApplicableTag(2, "High Priority",   None, "pri=high",        Live), Vector()),
      TagInTree(ApplicableTag(3, "Medium Priority", None, "pri=med",         Live), Vector()),
      TagInTree(TagGroup     (10, "Status",         None, MutexChildren.Not, Live), Vector(11.AT, 12.AT, 13.AT)),
      TagInTree(ApplicableTag(11, "WIP",            None, "wip",             Live), Vector()),
      TagInTree(ApplicableTag(12, "Deferred",       None, "defer",           Live), Vector()),
      TagInTree(ApplicableTag(13, "In UAT",         None, "uat",             Dead ), Vector()),
      TagInTree(TagGroup     (20, "Version",        None, MutexChildren.Not, Live), Vector(27.TG, 21.AT, 25.AT, 26.AT)),
      TagInTree(ApplicableTag(21, "v1.x",           None, "v1.x",            Live), Vector(22.AT, 23.AT, 24.AT)),
      TagInTree(ApplicableTag(22, "v1.0",           v10d, "v1.0",            Live), Vector()),
      TagInTree(ApplicableTag(23, "v1.1",           v11d, "v1.1",            Live), Vector()),
      TagInTree(ApplicableTag(24, "v1.2",           None, "v1.2",            Live), Vector()),
      TagInTree(ApplicableTag(25, "v2.x",           None, "v2.x",            Live), Vector()),
      TagInTree(ApplicableTag(26, "v3.x",           None, "v3.x",            Dead ), Vector()),
      TagInTree(TagGroup     (27, "Released",       None, MutexChildren.Not, Live), Vector(22.AT, 23.AT)),
      TagInTree(ApplicableTag(4, "Low Priority", Some("Nice to have. Stuff that probably won't be implemented."), "pri=low", Live), Vector()))


    lazy val fields = {
      import CustomField._
      RevAnd(40, FieldSet(emptyDataMap(CustomField).addAll(
        Text       (1, "Description", "desc",     Mandatory,     onlyReqTypes(2, 6, StaticReqType.UseCase), Live),
        Text       (2, "Notes",       "notes",    Mandatory.Not, notReqTypes(4),                            Live),
        Text       (3, "Reporter",    "reporter", Mandatory,     onlyReqTypes(5, StaticReqType.UseCase),    Dead),
        Tag        (4, 1.TG,                      Mandatory,     ISubset.All(),                             Live),
        Tag        (5, 10.TG,                     Mandatory.Not, ISubset.All(),                             Live),
        Implication(6, 2,                         Mandatory.Not, ISubset.All(),                             Live)
      ), Vector(
        Text.Id(1), Implication.Id(6), Tag.Id(4), Text.Id(3),
        StaticField.NormalAltStepTree, StaticField.ExceptionStepTree, StaticField.StepGraph,
        Tag.Id(5), Text.Id(2)
      )))
    }

    lazy val reqs     = RevAnd(40, Requirements.empty)
    lazy val reqCodes = RevAnd(50, ReqCodes.empty)
    lazy val reqText  = RevAnd(60, ReqData.emptyText)
    lazy val reqTags  = RevAnd(70, ReqData.emptyTags)
    lazy val reqImps  = RevAnd(80, Implications.empty)

    lazy val cfg = ProjectConfig(customIssueTypes, customReqTypes, fields, tagsR)
    lazy val project = Project(cfg, reqs, reqCodes, reqText, reqTags, reqImps)

    import shipreq.webapp.base.test.ProjectDsl._

    val List(co, mf, fr, br, dd, si) = List[CustomReqTypeId](1, 2, 3, 4, 5, 6)
    val List(p1,p3,p5,v10,wip,v1x,v3x,uat) = List[ApplicableTagId](4,3,2,22,11,21,26,13)
    val (p2,p4) = (p3,p5)
    val frs = (0 to 10).toVector.map(i => GenericReqId(i + 1000))
    val mfs = (0 to 28).toVector.map(i => GenericReqId(i + 1100))
    val cos = (0 to 10).toVector.map(i => GenericReqId(i + 1200))
    val sis = (0 to 10).toVector.map(i => GenericReqId(i + 1300))

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
      Vector(Issue(2, tbd), Literal(". "), ReqRef(mfs(28)), Literal(" is dead."))
    }

    val contentByDsl = (
      GReq(reqType = mf, id = mfs( 1), title = "Use Case Editor"                       , codes = Set("uce")).tag(p5).tag(v10)
    + GReq(reqType = mf, id = mfs( 2), title = "Anonymous Share"                       ).tag(p2).tag(v10)
    + GReq(reqType = mf, id = mfs( 3), title = "Export (PDF, XLS)"                     ).tag(p4)
    + GReq(reqType = mf, id = mfs( 4), title = "Templates"                             ).tag(p2)
    + GReq(reqType = mf, id = mfs( 5), title = "Field Customisation"                   ).tag(p5).tag(wip)
    + GReq(reqType = mf, id = mfs( 6), title = "Incompletions"                         ).tag(p3).tag(wip).tag(uat)
    + GReq(reqType = mf, id = mfs( 7), title = "Organisation"                          ).tag(p5).tag(wip).tag(v1x).tag(v10)
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
    + GReq(reqType = mf, id = mfs(19), title = "Glossary", live = Dead                 ).tag(p1)
    + GReq(reqType = mf, id = mfs(20), title = "Generic artifact storage"              ).tag(p3)
    + GReq(reqType = mf, id = mfs(21), title = "Doc authoring (V&S, URD, SRS)"         ).tag(p2)
    + GReq(reqType = mf, id = mfs(22), title = "High-level Requirements"               ).tag(p3).tag(wip)
    + GReq(reqType = mf, id = mfs(23), title = "Import external requirements"          ).tag(p2)
    + GReq(reqType = mf, id = mfs(24), title = "Requirement Lint"                      ).tag(p3)
    + GReq(reqType = mf, id = mfs(25), title = "Search"                                ).tag(p2)
    + GReq(reqType = mf, id = mfs(26), title = "Mass text modification (replace)"      ).tag(p1)
    + GReq(reqType = mf, id = mfs(27), title = "External references"                   ).tag(p1).tag(v3x)
    + GReq(reqType = mf, id = mfs(28), title = "Entities", live = Dead                 ).tag(p2).tag(v3x)

    + GReq(reqType = fr, id = frs(1), title = fr1Desc, codes = Set("uce.sample.1", "uce.sample.1b", "demo.whatever")).impSrc(mfs(12), mfs(19))
    + GReq(reqType = fr, id = frs(2), title = fr2Desc, codes = Set("uce.sample.2")).impSrc(mfs(1), mfs(13), mfs(22))
    + RCGroup("demo", Vector(T.ReqCodeGroupTitle.Literal("Demo group header")))

    + GReq(reqType = co, id = cos(1), live = Dead, title = "Search entities!").impSrc(mfs(28), mfs(25))
    + GReq(reqType = co, id = cos(2), live = Dead, title = "Entity-search should consider low-level reqs").impSrc(cos(1), frs(1))

    + GReq(reqType = si, id = sis(1), live = Dead, title = "Just use excel!").impSrc(mfs(12))

    + DeadReqCode("dead.ref", target = mfs(7))
    + DeadReqCode("dead.group")
    )

    contentByDsl ! project
  }

  var p = newProject

  implicit def blahblah[A](a: A): GenericFailure \/ A = \/-(a)

  val projectInit = ServerProtocol.routine(RemoteFns.ProjectInit)(_ => p)

  def emptyDelta = RemoteDelta.empty

  implicit def singleOptionalDelta(r: Option[RemoteDeltaPR]): RemoteDelta =
    r.fold(emptyDelta)(emptyDelta + _)

  // -------------------------------------------------------------------------------------------------------------------
  object reqqq {
    implicit val equality = Equal.equalA[CustomReqType]

    def upd(id: CustomReqTypeId, f: CustomReqType => CustomReqType) =
      mod(_.mod(id, f))

    def modR(f: CustomReqTypeIMap => CustomReqTypeIMap): Option[Rev] = {
      val c = p.config.customReqTypes
      val a = c.data
      val b = f(a)
      if (a ≟ b)
        None
      else {
        val newRev = c.rev.succ
        p = Project.customReqTypes.set(RevAnd(newRev, b))(p)
        Some(c.rev)
      }
    }

    def δ(p: Project) = p.config.customReqTypes.data.underlyingMap

    def mod(f: CustomReqTypeIMap => CustomReqTypeIMap): RemoteDelta = {
      val p1 = p
      delay()
      modR(f).map(oldRev => {
        val m1 = δ(p1)
        val m2 = δ(p)
        val delIds = m1.keySet -- m2.keySet
        val updates = m2.toStream.filter{ case (k,v) => !m1.contains(k) || m1(k) != v }.map(_._2).toList
        RemoteDeltaPR(Partition.CustomReqTypes, RevRange single oldRev)(delIds, updates)
      })
    }

    val crud =
      ServerProtocol.routine(RemoteFns.CustomReqTypeCrud)({
        case CrudAction.Create(v)    =>
          val (mnemonic, name, imp) = v
          val id = CustomReqTypeId(p.config.customReqTypes.data.keySet.max.value + 1)
          val n = CustomReqType(id, mnemonic, Set.empty, name, imp, Live)
          mod(_ + n)

        case CrudAction.Update(id, v) =>
          val (mnemonic, name, imp) = v
          upd(id, o => CustomReqType(id, mnemonic, (o.oldMnemonics + o.mnemonic) - mnemonic, name, imp, Live))

        case CrudAction.Delete(id, HardDel) => mod(_ - id)
        case CrudAction.Delete(id, SoftDel) => upd(id, _.copy(live = Dead))
        case CrudAction.Delete(id, Restore) => upd(id, _.copy(live = Live))
      })

    val imptoggle =
      ServerProtocol.routine(RemoteFns.ReqTypeImplicationMod){
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
      val c = p.config.customIssueTypes
      val a = c.data
      val b = f(a)
      if (a ≟ b)
        None
      else {
        val newRev = c.rev.succ
        p = Project.customIssueTypes.set(RevAnd(newRev, b))(p)
        Some(c.rev)
      }
    }

    def δ(p: Project) = p.config.customIssueTypes.data.underlyingMap

    def mod(f: CustomIssueTypeIMap => CustomIssueTypeIMap): RemoteDelta = {
      val p1 = p
      delay()
      modR(f).map(oldRev => {
        val m1 = δ(p1)
        val m2 = δ(p)
        val delIds = m1.keySet -- m2.keySet
        val updates = m2.toStream.filter{ case (k,v) => !m1.contains(k) || m1(k) != v }.map(_._2).toList
        RemoteDeltaPR(Partition.CustomIssueTypes, RevRange single oldRev)(delIds, updates)
      })
    }

    ServerProtocol.routine(RemoteFns.CustomIssueTypeCrud)({
      case CrudAction.Create(v)    =>
        val (key, desc) = v
        val id = CustomIssueTypeId(p.config.customIssueTypes.data.keySet.max.value + 1)
        val n = CustomIssueType(id, key, desc, Live)
        mod(_ + n)

      case CrudAction.Update(id, v) =>
        val (key, desc) = v
        upd(id, o => CustomIssueType(id, key, desc, Live))

      case CrudAction.Delete(id, HardDel) => mod(_ - id)
      case CrudAction.Delete(id, SoftDel) => upd(id, _.copy(live = Dead))
      case CrudAction.Delete(id, Restore) => upd(id, _.copy(live = Live))
    })
  }

  // -------------------------------------------------------------------------------------------------------------------
  object tagCrud {
    import data.{TagId => Id}
    import TagProtocol._

    def modR(f: TagTree => TagTree): Option[Rev] = {
      val rd = p.config.tags
      val a = rd.data
      val b = f(a)
      if (a ≟ b)
        None
      else {
        val newRev = rd.rev.succ
        p = Project.tags.set(RevAnd(newRev, b))(p)
        Some(rd.rev)
      }
    }

    def δ(p: Project): Map[Id, PovTag] = {
      val tt = p.config.tags.data
      val tree = tt.mapValues(_.children)
      tt.mapValues(v => PovTag(v.tag, MMTree.Relations.derive(v.tag.id, tree)))
    }

    // TODO Another copy/paste/search/replace
    def mod(f: TagTree => TagTree): RemoteDelta = {
      val p1 = p
      delay()
      modR(f).map(oldRev => {
        val m1 = δ(p1)
        val m2 = δ(p)
        val delIds = m1.keySet -- m2.keySet
        val updates = m2.toStream.filter{ case (k,v) => !m1.contains(k) || m1(k) != v }.map(_._2).toList
        RemoteDeltaPR(Partition.Tags, RevRange single oldRev)(delIds, updates)
      })
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
        or.fold(res)(MMTree.ApplyRelations.trustedApply1(res, i, _)) // TODO Possible cycle error
      })

    def nextId: TagId = TagGroupId(p.config.tags.data.keySet.map(_.value).max + 1)
    implicit def genIdToTG(g: TagId) = TagGroupId(g.value)
    implicit def genIdToAT(g: TagId) = ApplicableTagId(g.value)

    def build(i: Id): Values => Tag = {
      case TagGroupValues(n, mc, d)     => TagGroup(i, n, d, mc, Live)
      case ApplicableTagValues(n, k, d) => ApplicableTag(i, n, d, k, Live)
    }

    def setLife(t0: TagTree, id: Id, oa: Option[Live]): TagTree =
      t0.get(id).fold(t0){ subj =>

        // Modify children
        val t1 = subj.children.foldLeft(t0) { (t, childId) =>
          t.get(childId).map(_.tag) match {
            case Some(child) if child.live ≟ subj.tag.live =>
              val childToParents = Multimap(t.mapValues(_.children.toSet)).reverse // TODO I *HATE* performance!
              val hasLiveParent = (childToParents(childId) - id).exists(p => t.underlyingMap(p).tag.live ≟ Live)
              if (hasLiveParent)
                t
              else
                setLife(t, childId, oa)
            case _ => t
          }
        }

        // Modify subject
        oa match {
          case None    => t1.mapUnderlying(_.mapValuesNow(_ removeChild id) - id) // copy from RemoteDelta
          case Some(a) => updT(id, Tag.live set a)(t1)
        }
      }

    val fn = ServerProtocol.routine(RemoteFns.TagCrud)({
        case CrudAction.Create(v)           => put(nextId, v)
        case CrudAction.Update(i, v)        => put(i, v)
        case CrudAction.Delete(id, HardDel) => mod(setLife(_, id, None))
        case CrudAction.Delete(id, SoftDel) => mod(setLife(_, id, Some(Dead)))
        case CrudAction.Delete(id, Restore) => mod(setLife(_, id, Some(Live)))
      })
  }

  // -------------------------------------------------------------------------------------------------------------------
  object fieldCrud {
    import FieldProtocol._
    import CfgAction._
    import shipreq.webapp.base.data.{CustomField => CF, CustomFieldId => Id}

    def apply(deletions: Set[FieldId], updates: List[Delta]): RemoteDelta = {
      delay()
      val oldRev = p.config.fields.rev
      val rd = RemoteDeltaP(Partition.Fields)(deletions, updates)
      val p2 = ppi.update(p, oldRev.succ, rd)
      if (p.config.fields.data ≟ p2.config.fields.data)
        emptyDelta
      else {
        p = p2
        emptyDelta + RemoteDeltaPR(rd, RevRange single oldRev)
      }
    }

    def mod(f: FieldSet => List[Delta]): RemoteDelta =
      apply(Set.empty, f(p.config.fields.data))

    def mod(id: Id)(f: CF => CF): RemoteDelta =
      mod(fs => fs.customFields.get(id).fold(∅)(newField =>
        List(Delta(\/-(f(newField)), Position.get(fs.order, id)))))

    @inline def ∅ = List.empty[Delta]

    def nextId(fs: FieldSet): Id = CF.Text.Id(fs.customFields.keys.max.value + 1)
    implicit def genIdToText(g: Id) = CF.Text.Id(g.value)
    implicit def genIdToTag (g: Id) = CF.Tag.Id(g.value)
    implicit def genIdToImpl(g: Id) = CF.Implication.Id(g.value)

    val cfgAction =
      ServerProtocol.routine(RemoteFns.FieldCrud){

        case Create(TextFieldValues(n, k, m, r)) =>
          mod { fs =>
            val f = CF.Text(nextId(fs), n, k, m, r, Live)
            List(Delta(\/-(f), None))
          }

        case Create(TagFieldValues(t, m, r)) =>
          mod { fs =>
            val f = CF.Tag(nextId(fs), t, m, r, Live)
            List(Delta(\/-(f), None))
          }

        case Create(ImplicationFieldValues(t, m, r)) =>
          mod { fs =>
            val f = CF.Implication(nextId(fs), t, m, r, Live)
            List(Delta(\/-(f), None))
          }

        case UpdateValues(id, v) =>
          mod(id)(cf => (cf, v) match {
            case (CF.Text       (_, _, _, _, _, Live), TextFieldValues       (n, k, m, r)) => CF.Text       (id, n, k, m, r, Live)
            case (CF.Tag        (_,    _, _, _, Live), TagFieldValues        (t,    m, r)) => CF.Tag        (id, t,    m, r, Live)
            case (CF.Implication(_,    _, _, _, Live), ImplicationFieldValues(t,    m, r)) => CF.Implication(id, t,    m, r, Live)
            case _ => cf
          })

        case UpdateOrder(f: StaticField, p) =>
          mod(fs => List(Delta(-\/(f), p)))

        case UpdateOrder(id: Id, p) =>
          mod(_.customFields.get(id).fold(∅)(f => List(Delta(\/-(f), p))))

        case Delete(f: StaticField, Restore) =>
          mod(fs => if (fs.order contains f) Nil else List(Delta(-\/(f), None)))

        case Delete(id: Id, Restore) =>
          mod(id)(CF.live set Live)

        case Delete(f: StaticField, HardDel | SoftDel) =>
          f.deletable match {
            case Deletable     => apply(Set(f), Nil)
            case Deletable.Not => emptyDelta
          }

        case Delete(id: Id, SoftDel) =>
          mod(id)(CF.live set Dead)

        case Delete(id: Id, HardDel) =>
          apply(Set(id), Nil)
      }

    val mandmod =
      ServerProtocol.routine(RemoteFns.FieldMandatorinessMod){
        case (id, m) => mod(id)(CF.mandatory set m)
      }

  }

  // -------------------------------------------------------------------------------------------------------------------
  val updateProjectContent =
    ServerProtocol.routine(RemoteFns.UpdateProjectContent){ i =>
      println(s"RECEIVED: $i")
      delay()
      RemoteDelta.empty
    }

  // -------------------------------------------------------------------------------------------------------------------

  def delay(): Unit = Thread.sleep(300)
//  def delay(): Unit = Thread.sleep(new java.util.Random().nextInt(80)+80)

  def render = {
    val pg = RemoteFns.ProjectSPA(
      projectInit,
      issueTypeCrud,
      reqqq.crud, reqqq.imptoggle,
      fieldCrud.mandmod, fieldCrud.cfgAction,
      tagCrud.fn,
      updateProjectContent)
    val js = ServerProtocol.invokeClientHtml(JsEntryPoint.reactExamples)(pg)
    "*" #> js
  }
}
