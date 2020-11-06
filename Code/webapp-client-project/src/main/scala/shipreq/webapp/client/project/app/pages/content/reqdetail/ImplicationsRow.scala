package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.feature.TableNavigationFeature
import shipreq.webapp.client.project.app.Style.{reqdetail => *}
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.feature.EditorFeature.FieldKey
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.client.project.widgets.{EditorNavParent, _}
import shipreq.webapp.member.UiText
import shipreq.webapp.member.project.data._

private[reqdetail] object ImplicationsRow {
  import Row.{Implications => row}
  import EditorFeature.FieldKey.{Implications => Field}
  import Shared.tableNavigationFeature

  final case class Props(pubidText : String,
                         live      : Live,
                         editorF   : EditorFeature.ReadWrite.For[Field],
                         editorB   : EditorFeature.ReadWrite.For[Field],
                         editorArgs: EditorFeature.EditorArgs.ForAny,
                         view      : Reusable[ViewReq[VdomTag]],
                        ) {
    @inline def render: VdomElement = Component.withKey(row.key)(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private def render(p: Props): VdomNode =
    Shared.renderRow(
      row        = row,
      name       = UiText.FieldNames.implications,
      headerLive = Live,
      dataLive   = p.live,
    )(renderRowData(_, p))

  private val impRowSubBase =
    <.td(*.generalImpsSide, ^.tabIndex := -1)

  private def renderRowData(cell: Shared.DataCell, p: Props): VdomNode = {
    def renderHalf(dir: Direction) = {
      val editor = dir match {
        case Forwards  => p.editorF
        case Backwards => p.editorB
      }
      def key    = FieldKey.Implications.byDir(dir)
      def view   = p.view.editable(key).getOrElse(EmptyVdom)
      val f      = Field.byDir(dir)
      EditorNavParent.Props(impRowSubBase, editor, p.editorArgs(f), view).render
    }

    cell.nonDirectlyEditableNavParent(
      <.table(
        TableNavigationFeature.nestedTable,
        *.generalImpsCont,
        <.tbody(
          <.tr(
            renderHalf(Backwards),
            <.td(*.generalImpsMiddle, s"→ ${p.pubidText} →"),
            renderHalf(Forwards)))))
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
