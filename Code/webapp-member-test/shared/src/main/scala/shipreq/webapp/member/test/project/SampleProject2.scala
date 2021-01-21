package shipreq.webapp.member.test.project

import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.Atom.DisplayReqRef
import shipreq.webapp.member.project.text.{Text => T, _}
import shipreq.webapp.member.test.project.ProjectDsl._
import shipreq.webapp.member.test.project.SampleProject.{project => project0}
import shipreq.webapp.member.test.project.UnsafeTypes._

/**
 * Builds on SampleProject #1 to add some content with a tiny bit of rich text.
 *
 * Not extended by [[SampleProject3]].
 */
@nowarn("msg=match may not be exhaustive")
object SampleProject2 {

  lazy val project = {
    import SampleProject.Values._
    val List(p1,p3,p5) = List[ApplicableTagId](4,3,2)
    val (p2,p4) = (p3,p5)
    val mfs = (0 to 28).toVector.map(i => GenericReqId(i + 1000))

    def fr1Desc = {
      import T.GenericReqTitle._
      apply(
        EmailAddress("japgolly@gmail.com"), Literal(" is on "), WebAddress("https://github.com"),
        Literal(" cos of "), ReqRef(mfs(6), DisplayReqRef.AsId), Literal(" "), Issue(1, T.empty),
        TeX("c = \\pm\\sqrt{a^2 + b^2}")
      )
    }
    def fr2Desc = {
      val tbd = {
        import T.InlineIssueDesc._
        apply(Literal("Pending "), ReqRef(mfs(26), DisplayReqRef.AsId))
      }
      import T.GenericReqTitle._
      apply(Issue(2, tbd))
    }

    val contentByDsl = (
      GReq(reqType = mf, id = mfs( 1), title = "Use Case Editor"                       , codes = Set("uce")).tag(p5).tag(v10)
    + GReq(reqType = mf, id = mfs( 2), title = "Anonymous Share"                       ).tag(p2).tag(v10)
    + GReq(reqType = mf, id = mfs( 3), title = "Export (PDF, XLS)"                     ).tag(p4)
    + GReq(reqType = mf, id = mfs( 4), title = "Templates"                             ).tag(p2)
    + GReq(reqType = mf, id = mfs( 5), title = "Field Customisation"                   ).tag(p5).tag(wip)
    + GReq(reqType = mf, id = mfs( 6), title = "Incompletions"                         ).tag(p3).tag(wip)
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
    + RCGroup("demo", title = T.CodeGroupTitle(T.CodeGroupTitle.Literal("Demo group header")))
    )

    contentByDsl ! project0
  }

  lazy val plainText  = PlainText.ForProject.noCtx(project)
  lazy val textSearch = TextSearch(project, plainText)
}
