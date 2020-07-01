package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.TableNavigationFeature
import shipreq.webapp.client.project.app.Style.{reqdetail => *}
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.lib.EditorNavParent

private[reqdetail] object Shared {

  implicit val tableNavigationFeature = TableNavigationFeature.NoRowSpans

  val rowHeader: Live => VdomTag =
    Live.memo(l => <.th(*.detailTableKey(l)))

  val rowData: Live => VdomTag =
    Live.memo(l =>
      <.td(
        *.detailTableValue(l),
        ^.tabIndex := -1))

  def renderRow(row: Row, name: String, headerLive: Live, dataLive: Live)
               (renderData: DataCell => VdomNode): VdomTag = {
    val dataCell = new DataCell(rowData(dataLive))
    <.tr(
      ^.key := row.key,
      rowHeader(headerLive)(name),
      renderData(dataCell))
  }

  final class DataCell(private val cellBase: VdomTag) extends AnyVal {

    def editorNavParent[A](editor    : EditorFeature.ReadWrite.ForEditor[A, Any],
                           editorArgs: A,
                           view      : => TagMod) =
      EditorNavParent.Props(cellBase, editor, editorArgs, view).render

    def nonDirectlyEditableNavParent(t: TagMod): VdomElement =
      cellBase(tableNavigationFeature.onKeyDown, t)
  }
}
