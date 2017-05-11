package shipreq.webapp.client.project.app.reqtable2

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.UiText.EnglishIntExt
import shipreq.webapp.client.base.ui.semantic.Icon

/**
  * Presents a user with a summary of select portions of the content and state of [[ReqTablePage]].
  */
object PageSummary {

  final case class Props(table: TableContentStats, selectedRows: Int) {
    @inline def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.byRef || Reusability.caseClass

  private val iconReqs   = Icon.Cubes        .tag(^.title := "requirements")
  private val iconRCGs   = Icon.FolderOutline.tag(^.title := UiText.reqCodeGroups.toLowerCase)
  private val iconDelete = Icon.TrashOutline .tag(^.title := "deleted")
  private val iconFilter = Icon.Filter       .tag(^.title := "excluded by the filter")
  private val iconReapp  = Icon.Copy         .tag(^.title := "reappearances due to sorting")
  private val iconSelect = Icon.CheckmarkBox .tag(^.title := "selected", ^.marginRight := "0")

  private val fakeLine = <.span("_", ^.visibility.hidden)

  def summariseContent(table: TableContentStats): TagMod = {
    import table._

    val reqBreakdown: Option[TagMod] = {
      var parts = List.empty[TagMod]

      def add(n: Int, icon: VdomTag): Unit =
        if (n != 0) parts ::= TagMod(if (n < 0) s" - ${-n}" else s" + $n", icon)

      // Right-to-left
      add(reappearances, iconReapp)
      add(-reqsFilteredOut.all, iconFilter)
      add(reqsInProject.dead, iconDelete)

      Option.when(parts.nonEmpty)(
        TagMod(" (" + reqsInProject.live, parts.toTagMod, ")"))
    }

    val rcgs: Option[TagMod] =
      Option.when(codeGroups > 0)(
        TagMod(s" + $codeGroups", iconRCGs))

    /*
    val tableBreakdown: Option[TagMod] =
      Option.unless(reqBreakdown.isEmpty && rcgs.isEmpty)(
        TagMod(
          s": ${uniqueReqsInTable.all}", iconReqs,
          reqBreakdown.whenDefined,
          rcgs.whenDefined))

    TagMod(
      s"Showing ${totalRowsInTable.unitsOf("row")}",
      tableBreakdown.whenDefined,
      ".")
      */

    TagMod(
      s"Showing ${totalRowsInTable.unitsOf("row")}",
      s": ${uniqueReqsInTable.all}", iconReqs,
      reqBreakdown.whenDefined,
      rcgs.whenDefined,
      ".")
  }

  def summariseSelected(n: Int): Option[TagMod] =
    Option.when(n > 0)(
      TagMod(n, iconSelect, "."))

  def render(p: Props): VdomElement =
    <.div(
      summariseContent(p.table),
      <.br,
      summariseSelected(p.selectedRows) getOrElse fakeLine)

  val Component = ScalaComponent.builder[Props]("PageSummary")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}
