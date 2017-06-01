package shipreq.webapp.client.project.app.reqtable2

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.univeq.UnivEq
import shipreq.webapp.base.UiText
import shipreq.webapp.base.UiText.EnglishIntExt
import shipreq.webapp.base.data.{FilterDead, HideDead, ShowDead}
import shipreq.webapp.client.base.lib.DataReusability._
import shipreq.webapp.client.base.ui.semantic.Icon

/**
  * Presents a user with a summary of select portions of the content and state of [[ReqTablePage]].
  */
object PageSummary {

  final case class Props(table       : TableContentStats,
                         selectedRows: Int,
                         filterDead  : FilterDead) {
    @inline def render = Component(this)

    def tableFD: TableContentStats =
      filterDead match {
        case HideDead => table.clearDead
        case ShowDead => table
      }
  }

  object Props {
    implicit def equality: UnivEq[Props] =
      UnivEq.derive

    implicit val reusability: Reusability[Props] =
      Reusability.byRefOrUnivEq
  }

  private def icon(icon: Icon, title: String): VdomTag =
    icon.tag(^.title := title, ^.marginRight := "0")

  private val iconReqs   = icon(Icon.Cubes        , "requirements")(^.marginLeft := "0.1ex")
  private val iconRCGs   = icon(Icon.FolderOutline, UiText.codeGroups.toLowerCase)
  private val iconDelete = icon(Icon.TrashOutline , "deleted")
  private val iconFilter = icon(Icon.Filter       , "excluded by the filter")
  private val iconReapp  = icon(Icon.Copy         , "reappearances due to sorting")
  private val iconSelect = icon(Icon.CheckmarkBox , "selected")

  private val fakeLine = <.span("_", ^.visibility.hidden)

  private class Breakdown {
    private var parts = Vector.empty[TagMod]

    def add(as: TagMod*): Unit =
      parts ++= as

    def addUnlessZero(n: Int, icon: VdomTag): Unit =
      if (n != 0) {
        val txt: String =
//          if (parts.isEmpty)
//            n.toString
//          else
            if (n < 0) s" - ${-n}" else s" + $n"
        add(txt, icon)
      }

    def result: TagMod =
      TagMod.Composite(parts)

    def resultNonEmpty: Option[TagMod] =
      Option.when(parts.nonEmpty)(result)
  }

  def summariseContent(table: TableContentStats): TagMod = {
    import table._

    val reqBreakdown: Option[TagMod] = {
      val b = new Breakdown
      b.addUnlessZero(reqsInProject.dead, iconDelete)
      b.addUnlessZero(-reqsFilteredOut.all, iconFilter)
      b.resultNonEmpty.map(m => TagMod(" (", reqsInProject.live, m, ")"))
    }

    val rowBreakdown = new Breakdown
    rowBreakdown.add(uniqueReqsInTable.all, iconReqs, reqBreakdown.whenDefined)
    rowBreakdown.addUnlessZero(reappearances, iconReapp)
    rowBreakdown.addUnlessZero(codeGroups, iconRCGs)

    TagMod(s"Showing ${totalRowsInTable.unitsOf("row")}: ", rowBreakdown.result)
  }

  def summariseSelected(n: Int): Option[TagMod] =
    Option.when(n > 0)(
      TagMod(n, iconSelect, "."))

  def render(p: Props): VdomElement =
    <.div(
      summariseContent(p.tableFD),
      <.br,
      summariseSelected(p.selectedRows) getOrElse fakeLine)

  val Component = ScalaComponent.builder[Props]("PageSummary")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}
