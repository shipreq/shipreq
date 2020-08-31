package shipreq.webapp.client.project.lib

import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.filter._
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.pages.root.SpecialRouterCtl

final class Usage(p: Project, router: SpecialRouterCtl) {

  lazy val tags: LiveDeadStatMap[ApplicableTagId, Int] = {
    val tags = p.virtualTags
    val b = LiveDeadStatMap.Builder.ofInts[ApplicableTagId]()
    for {
      req    <- p.content.reqs.reqIterator()
      reqLive = req.live(p.config.reqTypes)
      tagId  <- tags(req.id, ShowDead).set(TagFieldId.All)
    } {
      val live = reqLive & p.config.tags.needApplicableTag(tagId).live
      b(tagId).add(live, 1)
    }
    b.result()
  }

  def tagLink(id: ApplicableTagId, fd: FilterDead): VdomTagOf[html.Anchor] = {
    val uses = tags(id)(fd)
    val rc   = router.reqTableWithFilter(fd, Filter.Valid.tag(id))
    rc.link(())(Usage.render(uses))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private val filterCompiler: FilterDead => Filter.Valid.Compiler =
    FilterDead.memoLazy(fd =>
      Filter.Valid.compiler(
        p,
        PlainText.ForProject.noCtx.empty,
        TextSearch.empty,
        fd,
        applyFilterDeadToReqs = true
      ))

  private def fieldFilter(fid: FieldId): Filter.Valid = {
    import FilterAst.FieldAttr._
    import FilterAst.FieldCriteria.Attr
    import Filter.Valid._
    fieldProp(\/-(fid), Attr(NotBlank))
  }

  val fields: FilterDead => FieldId => Int =
    FilterDead.memoLazy { fd =>
      val compiler = filterCompiler(fd)
      Memo { fid =>
        val filter = compiler(fieldFilter(fid)).req.toFn
        p.content.reqs.reqIterator().count(filter)
      }
    }

  def fieldLink(id: FieldId, fd: FilterDead): VdomTagOf[html.Anchor] = {
    val uses = fields(fd)(id)
    val rc   = router.reqTableWithFilter(fd, fieldFilter(id))
    rc.link(())(Usage.render(uses))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def reqTypeLink(id: ReqTypeId, fd: FilterDead): VdomTagOf[html.Anchor] = {
    val uses = p.reqTypeCount(id)(fd)
    val rc   = router.reqTableWithFilter(fd, Filter.Valid.reqType(id))
    rc.link(())(Usage.render(uses))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def customIssueTypeLink(id: CustomIssueTypeId, fd: FilterDead): VdomTagOf[html.Anchor] = {
    val uses = p.atomScan.issueCounts(id)(fd)
    val rc   = router.reqTableWithFilter(fd, Filter.Valid.issue(id))
    rc.link(())(Usage.render(uses))
  }
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object Usage {

  def apply(p: Project, router: SpecialRouterCtl): Usage =
    new Usage(p, router)

  implicit def reusability: Reusability[Usage] =
    Reusability.byRef

  // ===================================================================================================================

  private val hiddenS =
    <.span(^.visibility.hidden, "s")

  def render(uses: Int): TagMod =
    TagMod(
      TagMod.when(uses == 0)(Style.usageZero),
      if (uses == 1)
        TagMod(s"$uses use", hiddenS)
      else
        s"$uses uses",
    )

}
