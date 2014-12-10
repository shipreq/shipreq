package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._

object UiLib {

  def rowStatusRowClass(rs: RowStatus): String = rs match {
    case RowStatus.Sync      => "sync"
    case RowStatus.Locked    => "locked"
    case RowStatus.Failed(_) => "failed"
  }

  def rowStatusCtrls(rs: RowStatus, ctrls: => Modifier): Modifier =
    rowStatusCtrlsFold(rs, ctrls, t => t, t => t)

  def rowStatusCtrlsFold(rs: RowStatus, sync: => Modifier, locked: Tag => Modifier, failed: Tag => Modifier): Modifier = rs match {
    case RowStatus.Sync      => sync
    case RowStatus.Locked    => locked(spinner)
    case RowStatus.Failed(r) => failed(button("Retry", onclick ~~> r))
  }

  def spinner =
    img(cls := "spinner", src := "/assets/loading-spin.svg")

}
