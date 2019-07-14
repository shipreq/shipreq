package shipreq.webapp.client.project.app.issues

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import scalaz.\/
import shipreq.base.util.ConsolidatedSeq
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{HideDead, ReqCode, ReqId}
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.feature.editor.FieldKey
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets

object TableRow {
  type TD = VdomTagOf[html.TableCell]

  final case class Props(row             : Row,
                         columns         : NonEmptyVector[Column],
                         editor          : EditorFeature.ReadWrite.ForProject,
                         pxProjectWidgets: Reusable[Px[ProjectWidgets.NoCtx]],
                         pubidFormat     : ProjectWidgets.NoCtx#PubidFormat,
                         issueCategory   : Option[Reusable[TD]],
                         issueClass      : Option[Reusable[TD]],
                         idBase          : Option[Reusable[TD]],
                         titleBase       : Option[Reusable[TD]],
                        )

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val td = <.td(*.tableData)

  private val na = TagMod(*.na, "–")

  private type RenderGroup[-A] = ConsolidatedSeq.Group[A] => Reusable[TD]

  private def renderGroupBase(col: Column): RenderGroup[Any] =
    g => Reusable.byRef(td(^.key := col.key, ^.rowSpan := g.size))

  private def renderIssueGroup(col: Column): RenderGroup[String] = g => {
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

  val consolidateIssueClasses = consolidateStrings(renderIssueGroup(Column.IssueClass))

  final case class Id(group: Int, value: Option[ReqCode.Value \/ ReqId])

  object Id {
    implicit def univEq: UnivEq[Id] = UnivEq.derive
    val consolidate = ConsolidatedSeq.Logic.cmp[Id]((a, b) => a.value.isDefined && (a ==* b))(renderGroupBase(Column.Id))
  }

  private val consolidateText = ConsolidatedSeq.Logic.consolidateByUnivEq[(Int, Text.AnyOptional)]

  val consolidateTitle = consolidateText(renderGroupBase(Column.Title))

  private def render(p: Props): VdomElement = {
    import p.{row, pubidFormat}

    val cells = VdomArray.empty()

    for (col <- p.columns) {
      def addTD(content: TagMod) = cells += td(^.key := col.key, content)

      col match {

        case Column.IssueCategory =>
          p.issueCategory.foreach(cells += _.value)

        case Column.IssueClass =>
          p.issueClass.foreach(cells += _.value)

        case Column.FieldName =>
          addTD(row.fieldOption.fold(na)(_.desc))

        case Column.FieldEditor =>
          addTD(row match {

            case r: Row.ForGenericReq =>
              p.editor
                .forGenericReq(r.req.id)(r.field.key, p.pxProjectWidgets, HideDead)
                .themedRenderOr(())(r.renderer(r.field.key))

            case r: Row.ForUseCase =>
              p.editor
                .forUseCase(r.req.id)(r.field.key, p.pxProjectWidgets, HideDead)
                .themedRenderOr(())(r.renderer(r.field.key))

            case r: Row.ForUseCaseStep =>
              p.editor
                .forUseCaseSteps(r.field.key, p.pxProjectWidgets, HideDead)
                .themedRenderOr(FieldKey.UseCaseStep.Args.empty)(r.renderer(r.field.key))

            case r: Row.ForRcg =>
              r.fieldOption match {
                case None => na
                case Some(f) =>
                  p.editor
                    .forCodeGroup(r.rcg.id)(f.key, p.pxProjectWidgets, HideDead)
                    .themedRenderOr(())(r.renderer(f.key))
              }

            case _: Row.ForConfig =>
              na
          })

        case Column.Actions =>
          addTD("TODO") // TODO ==========================

        case Column.Id =>
          for (base <- p.idBase) {
            val c = row match {
              case r: Row.ForReq    => pubidFormat(r.req)
              case r: Row.ForRcg    => r.renderer(FieldKey.Code)
              case _: Row.ForConfig => na
            }
            cells += base(c)
          }

        case Column.Title =>
          for (base <- p.titleBase) {
            val c = row match {
              case r: Row.ForGenericReq  => r.renderer(FieldKey.GenericReqTitle)
              case r: Row.ForUseCase     => r.renderer(FieldKey.UseCaseTitle)
              case r: Row.ForUseCaseStep => r.ucRenderer(FieldKey.UseCaseTitle)
              case r: Row.ForRcg         => r.renderer(FieldKey.CodeGroupTitle)
              case _: Row.ForConfig      => na
            }
            cells += base(c)
          }

        case Column.Code =>
          addTD("TODO") // TODO ==========================

        case Column.ReqType =>
          addTD("TODO") // TODO ==========================

        case Column.Tags =>
          addTD("TODO") // TODO ==========================

        case Column.Implications(dir) =>
          addTD("TODO") // TODO ==========================

        case Column.CustomField(id) =>
          addTD("TODO") // TODO ==========================
      }
    }

    <.tr(cells)
  }

  val Component = ScalaComponent.builder[Props]("TableRow")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}
