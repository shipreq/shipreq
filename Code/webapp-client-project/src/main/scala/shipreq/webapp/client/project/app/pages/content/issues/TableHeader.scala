package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{FieldId, SpecialBuiltInField}
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
      case Column.Id                => SpecialBuiltInField.Pubid.name
      case Column.Title             => SpecialBuiltInField.Title.name
//      case Column.Code              =>
//      case Column.ReqType           =>
//      case Column.Tags              =>
//      case Column.Implications(dir) =>
//      case Column.CustomField(id)   =>
    }

    <.thead(
      <.tr(
        p.columns.whole.toVdomArray(c =>
          <.th(
            *.tableHeader,
            ^.key := c.key,
            columnName(c)))))
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build
}