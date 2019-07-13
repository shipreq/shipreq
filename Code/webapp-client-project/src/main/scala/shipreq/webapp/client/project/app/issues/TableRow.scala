package shipreq.webapp.client.project.app.issues

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util.ConsolidatedSeq
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

object TableRow {
  type TD = VdomTagOf[html.TableCell]

  final case class Props(row          : Row,
                         columns      : NonEmptyVector[Column],
                         pw           : ProjectWidgets.AnyCtx,
                         pubidFormat  : ProjectWidgets.AnyCtx#PubidFormat,
                         issueCategory: Option[Reusable[TD]],
                         issueClass   : Option[Reusable[TD]])

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val td = <.td(*.tableData)

  private val na = td(*.na, "–")

  private def renderIssueGroup(col: Column): ConsolidatedSeq.Group[String] => Reusable[TD] = g => {
    val base = td(^.key := col.key, g.value)
    val result =
      if (g.size == 1)
        base
      else
        base(
          <.div(*.rowspanOuter, "(", <.span(*.rowspanInner, g.size), ")"),
          ^.rowSpan := g.size)
    Reusable.byRef(result)
  }

  private val consolidateStrings = ConsolidatedSeq.Logic.consolidateByUnivEq[String]
  val consolidateIssueCategories = consolidateStrings(renderIssueGroup(Column.IssueCategory))
  val consolidateIssueClasses    = consolidateStrings(renderIssueGroup(Column.IssueClass))

  private def render(p: Props): VdomElement = {
    import p.{row, pw, pubidFormat}

    val cells = VdomArray.empty()

    for (col <- p.columns) {

      val content: TagMod =
        col match {

          case Column.IssueCategory =>
            p.issueCategory.foreach(cells += _.value)
            null

          case Column.IssueClass =>
            p.issueClass.foreach(cells += _.value)
            null

          case Column.FieldName =>
            row.fieldKeyOption.fold("")(_.toString)

          case Column.FieldEditor =>
            "TODO" // TODO ==========================

          case Column.Actions =>
            "TODO" // TODO ==========================

          case Column.Id =>
            row match {
              case r: Row.ForReq    => pubidFormat(r.req)
              case r: Row.ForRcg    => pw.reqCode(r.code)
              case _: Row.ForConfig => na
            }

          case Column.Title =>
            row match {
              case r: Row.ForReq    => r.viewReq(pw).title
              case r: Row.ForRcg    => pw.codeGroupTitle(r.rcg)
              case _: Row.ForConfig => na
            }

          case Column.Code =>
            "TODO" // TODO ==========================

          case Column.ReqType =>
            "TODO" // TODO ==========================

          case Column.Tags =>
            "TODO" // TODO ==========================

          case Column.Implications(dir) =>
            "TODO" // TODO ==========================

          case Column.CustomField(id) =>
            "TODO" // TODO ==========================
        }

      if (content ne null)
        cells += td(^.key := col.key, content)
    }

    <.tr(cells)
  }

  val Component = ScalaComponent.builder[Props]("TableRow")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}
