package shipreq.webapp.base.test

import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.text.{Text => T, _}
import shipreq.webapp.base.test.ProjectDsl._
import shipreq.webapp.base.test.UnsafeTypes._
import SampleProject.{project => project0, _}

/**
 * Builds on SampleProject #1 (not #2) to add:
 *   - generic reqs (dead & live)
 *   - req code groups
 *   - dead req codes
 *   - a bit of rich text.
 *   - deletion reasons.
 */
object SampleProject3 {

  trait Values extends SampleProject.Values {
    val frs = (0 to 10).iterator.map(i => GenericReqId(i + 1000)).toVector
    val mfs = (0 to 28).iterator.map(i => GenericReqId(i + 1100)).toVector
    val cos = (0 to 10).iterator.map(i => GenericReqId(i + 1200)).toVector
  }
  object Values extends Values
  import Values._

  val inlineIssueDesc = {
    import T.InlineIssueDesc._
    Vector(Literal("Pending "), ReqRef(mfs(26)))
  }

  lazy val project: Project = {
    import SampleProject.Values._
    val List(p1,p3,p5) = List[ApplicableTagId](4,3,2)
    val (p2,p4) = (p3,p5)

    def fr1Desc = {
      import T.GenericReqTitle._
      Vector(
        EmailAddress("japgolly@gmail.com"), Literal(" is on "), WebAddress("https://github.com"),
        Literal(" cos of "), ReqRef(mfs(6)), Literal(" "), Issue(1, Vector.empty),
        TeX("c = \\pm\\sqrt{a^2 + b^2}")
      )
    }
    def fr2Desc = {
      import T.GenericReqTitle._
      Vector(Issue(2, inlineIssueDesc), Literal(". "), ReqRef(mfs(28)), Literal(" is dead."))
    }

    val contentByDsl = (
      GReq(reqType = mf, id = mfs( 1), title = "Use Case Editor"                      , codes = Set("uce")).tag(p5).tag(v10)
    + GReq(reqType = mf, id = mfs( 2), title = "Anonymous Share"                      ).tag(p2).tag(v10)
    + GReq(reqType = mf, id = mfs( 3), title = "Export (PDF, XLS)"                    ).tag(p4)
    + GReq(reqType = mf, id = mfs( 4), title = "Templates"                            ).tag(p2)
    + GReq(reqType = mf, id = mfs( 5), title = "Field Customisation"                  ).tag(p5).tag(wip)
    + GReq(reqType = mf, id = mfs( 6), title = "Incompletions"                        ).tag(p3).tag(wip)
    + GReq(reqType = mf, id = mfs( 7), title = "Organisation"                         ).tag(p5).tag(wip).tag(v1x).tag(v10)
    + GReq(reqType = mf, id = mfs( 8), title = "History/Audit"                        ).tag(p3)
    + GReq(reqType = mf, id = mfs( 9), title = "Collaboration: authoring"             ).tag(p5)
    + GReq(reqType = mf, id = mfs(10), title = "Collaboration: stakeholders"          ).tag(p5)
    + GReq(reqType = mf, id = mfs(11), title = "Collaboration: change mgnt & approval").tag(p4)
    + GReq(reqType = mf, id = mfs(12), title = "Low-level Requirements"               ).tag(wip).tag(p5)
    + GReq(reqType = mf, id = mfs(13), title = "Requirement Relationships"            ).tag(wip).tag(p4)
    + GReq(reqType = mf, id = mfs(14), title = "Text-generated Diagrams"              ).tag(p2)
    + GReq(reqType = mf, id = mfs(15), title = "Matrixes"                             ).tag(p2)
    + GReq(reqType = mf, id = mfs(16), title = "CRUDL Matrix"                         ).tag(p1)
    + GReq(reqType = mf, id = mfs(17), title = "Undo & Auto-save"                     ).tag(p2)
    + GReq(reqType = mf, id = mfs(18), title = "Data dictionary"                      ).tag(p1)
    + GReq(reqType = mf, id = mfs(19), title = "Glossary", live = Dead                ).tag(p1)
    + GReq(reqType = mf, id = mfs(20), title = "Generic artifact storage"             ).tag(p3)
    + GReq(reqType = mf, id = mfs(21), title = "Doc authoring (V&S, URD, SRS)"        ).tag(p2)
    + GReq(reqType = mf, id = mfs(22), title = "High-level Requirements"              ).tag(p3).tag(wip)
    + GReq(reqType = mf, id = mfs(23), title = "Import external requirements"         ).tag(p2)
    + GReq(reqType = mf, id = mfs(24), title = "Requirement Lint"                     ).tag(p3).tag(v3x)
    + GReq(reqType = mf, id = mfs(25), title = "Search"                               ).tag(p2)
    + GReq(reqType = mf, id = mfs(26), title = "Mass text modification (replace)"     ).tag(p1)
    + GReq(reqType = mf, id = mfs(27), title = "External references"                  ).tag(p1).impSrc(frs(2))
    + GReq(reqType = mf, id = mfs(28), title = "Entities", live = Dead                ).tag(p2)

    + GReq(reqType = fr, id = frs(1), title = fr1Desc, codes = Set("uce.sample.1", "uce.sample.1b", "demo.whatever")).impSrc(mfs(12), mfs(19))
    + GReq(reqType = fr, id = frs(2), title = fr2Desc, codes = Set("uce.sample.2")).impSrc(mfs(1), mfs(13), mfs(22), frs(1))
    + RCGroup("demo", title = Vector(T.CodeGroupTitle.Literal("Demo group header")))

    + GReq(reqType = co, id = cos(1), live = Dead, title = "Search entities!").impSrc(mfs(28), mfs(25)).tag(v10, v3x)
    + GReq(reqType = co, id = cos(2), live = Dead, title = "Entity-search should consider low-level reqs").impSrc(cos(1), frs(1))

    + DeadReqCode("dead.ref", oldReqId = mfs(7))
    + DeadReqCode("dead.group")
    )

    val dr = DeletionReasons(
      Vector("Who needs a use case edtior?!", "Bobsaidso.", "Bob said so."),
      DeletionReasons.emptyReqApplication
        .add(mfs(1), 0)
        .add(cos(2), None)
        .add(cos(2), 1)
        .add(cos(2), 2)
    )

    Project.deletionReasons.set(dr)(contentByDsl ! project0)
  }

  lazy val plainText  = PlainText.ForProject.noCtx(project)
  lazy val textSearch = TextSearch(project, plainText)
}
