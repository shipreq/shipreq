package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets._

private[reqdetail] object LifeRow {
  import Row.{Life => row}

  final case class Props(reqId          : ReqId,
                         live           : Live,
                         allowLiveChange: Permission,
                         delete         : Reusable[ReqId => Callback],
                         restore        : Reusable[ReqId => Callback],
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private def render(p: Props): VdomNode =
    Shared.renderRow(
      row        = row,
      name       = UiText.Life.field,
      headerLive = Live,
      dataLive   = Live, // When req is dead, [Restore] should be highlighted is active
    )(renderRowData(_, p))

  private def renderRowData(cell: Shared.DataCell, p: Props): VdomNode =
    cell.nonDirectlyEditableNavParent(
      p.live match {
        case Live =>
          LifeButton.Delete withStatusOnLeft p.delete(p.reqId)
        case Dead =>
          LifeButton.Restore.withStatusOnLeft(
            p.allowLiveChange option p.restore(p.reqId))
      })

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
