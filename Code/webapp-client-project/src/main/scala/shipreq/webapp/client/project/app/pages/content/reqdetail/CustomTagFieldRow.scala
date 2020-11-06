package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.client.project.app.Style.{reqdetail => *}
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.feature.EditorFeature.FieldKey
import shipreq.webapp.client.project.widgets._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.util.DataReusability._

private[reqdetail] object CustomTagFieldRow {
  import EditorFeature.FieldKey.{CustomFieldTags => Field}

  final case class Props(reqId     : ReqId,
                         fieldId   : CustomField.Tag.Id,
                         name      : String,
                         headerLive: Live,
                         dataLive  : Live,
                         editor    : EditorFeature.ReadWrite.For[Field],
                         editorArgs: Field#Args,
                         view      : Reusable[ViewReq[VdomTag]],
                         project   : Project,
                        ) {
    val row = Row.CustomField(fieldId)
    @inline def render: VdomElement = Component.withKey(row.key)(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private def render(p: Props): VdomNode =
    Shared.renderRow(
      row        = p.row,
      name       = p.name,
      headerLive = p.headerLive,
      dataLive   = p.dataLive,
    )(renderRowData(_, p))

  private def renderRowData(cell: Shared.DataCell, p: Props): VdomNode = {
    val field      = FieldKey.CustomFieldTags(p.fieldId)
    def basicView  = p.view.editable(field).getOrElse(EmptyVdom)
    val pbPortions = p.project.virtualTags(p.reqId).childrenSummary(p.fieldId).progressBar

    def view =
      if (pbPortions.isEmpty)
        basicView

      else {
        val item = <.span(*.progressBarItem)
        val bar =
          <.span(*.progressBar,
            pbPortions.toTagMod { p =>
              item(
                p.tag.whenDefined(t => ^.backgroundColor := t.colour.getOrElse(Colour.tagDefault).value),
                ^.width := p.pct100s,
                ^.title := p.desc,
              )
            }
          )

        <.div(*.derivativeTagRow,
          <.div(*.derivativeTagRowTags, basicView),
          <.div(*.derivativeTagRowBar, bar))
      }

    cell.editorNavParent(p.editor, p.editorArgs, view)
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
