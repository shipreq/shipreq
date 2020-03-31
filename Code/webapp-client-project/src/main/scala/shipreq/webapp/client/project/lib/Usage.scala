package shipreq.webapp.client.project.lib

import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import scalaz.\/-
import scalaz.std.anyVal.intInstance
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter._
import shipreq.webapp.base.text.{PlainText, TextSearch}
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.app.pages.root.SpecialRouterCtl

final class Usage(p: Project, router: SpecialRouterCtl) {

  lazy val tags: LiveDeadStatMap[ApplicableTagId, Int] = {
    val tagLookup = p.dataLogic.tagLookup(ShowDead)
    val b = new LiveDeadStatMap.Builder[ApplicableTagId, Int]
    for {
      req    <- p.content.reqs.reqIterator()
      reqLive = req.live(p.config.reqTypes)
      tagId  <- tagLookup(req.id).all
    } {
      val live = reqLive & p.config.tags.needApplicableTag(tagId).live
      b(tagId).mod(live)(_ + 1)
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

  private def fieldFilter(fid: CustomFieldId): Filter.Valid = {
    import FilterAst.FieldAttr._
    import Filter.Valid._
    val f = \/-(fid)
    not(anyOf(fieldProp(f, Blank), fieldProp(f, NotApplicable)))
  }

  val fields: FilterDead => CustomFieldId => Int =
    FilterDead.memoLazy { fd =>
      val compiler = filterCompiler(fd)
      Memo { fid =>
        val filter = compiler(fieldFilter(fid)).req.toFn
        p.content.reqs.reqIterator().count(filter)
      }
    }

  def fieldLink(id: CustomFieldId, fd: FilterDead): VdomTagOf[html.Anchor] = {
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
        TagMod(uses + " use", hiddenS)
      else
        uses + " uses",
    )

}
