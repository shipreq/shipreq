package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import scalaz.{Equal, \/, \&/, -\/, \/-}, \&/._
import scalaz.syntax.equal._
import shipreq.base.util._
import shipreq.base.util.ScalaExt._
import japgolly.nyaya.util.{Util => _, _}
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.data, data._, DataImplicits._
import shipreq.webapp.base.event._
import shipreq.webapp.base.text.{Text => T}
import shipreq.webapp.server.protocol.ServerProtocol
import shipreq.webapp.server.util.QuietException

class WIP {

  // =============================================================================
  // No input should be trusted! Incoming deltas should be validated!
  // Needless updates should be avoided.
  // =============================================================================

  def newProject = {
    import shipreq.webapp.base.data._
    import shipreq.webapp.base.test.UnsafeTypes._

    val customIssueTypes = emptyDataMap(CustomIssueType).addAll(
      CustomIssueType(1, "TO"+"DO", "Something you need To Do.", Live),
      CustomIssueType(2, "TBD", "To Be Decided.", Live))

    val customReqTypes = emptyDataMap(CustomReqType).addAll(
      CustomReqType(1, "CO", Set.empty, "Constraint",             ImplicationRequired.Not, Live),
      CustomReqType(2, "MF", Set.empty, "Major Feature",          ImplicationRequired.Not, Live),
      CustomReqType(3, "FR", Set.empty, "Functional Requirement", ImplicationRequired,     Live),
      CustomReqType(4, "BR", Set.empty, "Business Rule",          ImplicationRequired.Not, Live),
      CustomReqType(5, "DD", Set("DA", "DDF"), "Data Definition", ImplicationRequired.Not, Dead),
      CustomReqType(6, "SI", Set.empty, "Solution Idea",          ImplicationRequired,     Dead))

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
      FieldSet(emptyDataMap(CustomField).addAll(
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
      ))
    }

    lazy val reqs     = Requirements.empty
    lazy val reqCodes = ReqCodes.empty
    lazy val reqText  = ReqData.emptyText
    lazy val reqTags  = ReqData.emptyTags
    lazy val reqImps  = Implications.empty

    lazy val cfg = ProjectConfig(customIssueTypes, customReqTypes, fields, tags)
    lazy val project = IdCeilings.supply(Project(cfg, reqs, reqCodes, reqText, reqTags, reqImps, _))

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

  val noChange = \/-(Vector.empty[VerifiedEvent])

  def updateProject(f: UpdateProject.State => UpdateProject.UpdateResult): GenericFailure \/ VerifiedEvents =
    f(UpdateProject.initState(p)) match {
      case UpdateProject.Updated(s2, ves) =>
        p = s2.project
        \/-(ves)
      case UpdateProject.NoChange =>
        noChange
      case UpdateProject.Failed(err) =>
        -\/(GenericFailure(err))
    }

  implicit def blahblah[A](a: A): GenericFailure \/ A = \/-(a)

  val projectInit = ServerProtocol.remoteFn(ProjectInit)(_ => p)

  // -------------------------------------------------------------------------------------------------------------------
  object reqqq {

    val crud =
      ServerProtocol.remoteFn(CustomReqTypeCrud)(input =>
        updateProject(UpdateProject.customReqTypeCrud(input, _)))

    val imptoggle =
      ServerProtocol.remoteFn(ReqTypeImplicationMod)(input =>
        updateProject(UpdateProject.reqTypeImplicationMod(input, _)))
  }

  // -------------------------------------------------------------------------------------------------------------------
  val issueTypeCrud =
    ServerProtocol.remoteFn(CustomIssueTypeCrud)(input =>
      updateProject(UpdateProject.customIssueTypeCrud(input, _)))

  // -------------------------------------------------------------------------------------------------------------------
  val tagCrud =
    ServerProtocol.remoteFn(TagCrud.Fn)(input =>
      updateProject(UpdateProject.tagCrud(input, _)))

  // -------------------------------------------------------------------------------------------------------------------
  object fieldCrud {
    val cfgAction =
      ServerProtocol.remoteFn(FieldCrud.Fn)(input =>
        updateProject(UpdateProject.fieldCrud(input, _)))

    val mandmod =
      ServerProtocol.remoteFn(FieldMandatorinessMod)(input =>
        updateProject(UpdateProject.fieldMandatorinessMod(input, _)))
  }

  // -------------------------------------------------------------------------------------------------------------------
  val updateProjectContent =
    ServerProtocol.remoteFn(ContentUpdate.Fn){ i =>
      println(s"RECEIVED: $i")
      delay()
      Vector.empty: VerifiedEvents
    }

  // -------------------------------------------------------------------------------------------------------------------

  def delay(): Unit = Thread.sleep(300)
//  def delay(): Unit = Thread.sleep(new java.util.Random().nextInt(80)+80)

  def render = {
    val pg = ProjectSPA(
      projectInit,
      issueTypeCrud,
      reqqq.crud, reqqq.imptoggle,
      fieldCrud.mandmod, fieldCrud.cfgAction,
      tagCrud,
      updateProjectContent)
    val js = ServerProtocol.invokeClientHtml(JsEntryPoint.reactExamples)(pg)
    "*" #> js
  }
}
