package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util.UnivEq
import shipreq.webapp.base.data._
import shipreq.webapp.client.data.DataReusability._

object SelectionCtrls {

  case class Props(sel: RowSelectionVisible, cfg: ProjectConfig, rows: Rows)

//  implicit def equalProps: UnivEq[Props]      = UnivEq.derive
  implicit def reuseProps: Reusability[Props] = Reusability.caseClass

  class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props) = {

      val selCount = p.sel.visibleSelection.size

      var deletable  = 0
      var restorable = 0

      if (selCount != 0) {
        var remaining = p.sel.visibleSelection // because the same sourceId can appear more than once
        p.rows foreach { row =>
          val id = row.sourceId
          if (remaining contains id) {
            remaining -= id
            row match {
              case r: GenericReqRow =>
                r.live match {
                  case Live => deletable += 1
                  case Dead => if (r.req.recoverable(p.cfg.customReqTypes)) restorable += 1
                }
              case r: ReqCodeGroupRow =>
                r.live match {
                  case Live => deletable += 1
                  //case Dead => if (r.group.recoverable) restorable += 1
                }
            }
          }
        }
      }

      def infoText =
        selCount match {
          case 0 => "0 items selected."
          case 1 => "1 item selected."
          case n => n.toString + " items selected."
        }

      def button(count: Int, label: String) =
        if (count == 0)
          <.button(^.disabled := true, label)
        else
          <.button(if (count == selCount) label else s"$label ($count)")


      val delButton = button(deletable, "Delete")
      val resButton = button(restorable, "Restore")

      <.div(infoText, delButton, resButton)
    }
  }

  val Component = ReactComponentB[Props]("SelCtrls")
    .renderBackend[Backend]
    .configure(shouldComponentUpdate)
    .build
}
