package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.FieldId
import shipreq.webapp.base.lib.BaseReusability._
import shipreq.webapp.client.project.app.Style.{issues => *}

object TableHeader {

  final case class Props(columns: NonEmptyVector[Column],
                         fieldNames: FieldId ~=> String) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private def render(p: Props): VdomElement = {

    val columnName: Column => String = {
      case Column.IssueCategory     => UiText.ColumnNames.issueCategory
      case Column.IssueClass        => UiText.ColumnNames.issueClass
      case Column.FieldName         => UiText.ColumnNames.issueFieldName
      case Column.FieldEditor       => UiText.ColumnNames.issueFieldEditor
      case Column.Actions           => UiText.ColumnNames.issueActions
      case Column.Id                => UiText.ColumnNames.id
      case Column.Title             => UiText.ColumnNames.title
//      case Column.Code              => UiText.ColumnNames.code
//      case Column.ReqType           => UiText.ColumnNames.reqType
//      case Column.Tags              => UiText.ColumnNames.tags
//      case Column.Implications(dir) => UiText.ColumnNames.implications(dir)
//      case Column.CustomField(id)   => p.fieldNames(id)
    }

    <.thead(
      <.tr(
        p.columns.whole.toVdomArray(c =>
          <.th(
            *.tableHeader,
            ^.key := c.key,
            columnName(c)))))
  }

  val Component = ScalaComponent.builder[Props]("TableHeader")
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}