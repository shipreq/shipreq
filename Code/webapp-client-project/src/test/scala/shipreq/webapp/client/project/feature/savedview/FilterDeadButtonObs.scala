package shipreq.webapp.client.project.feature.savedview

import japgolly.scalajs.react.test.Simulate
import org.scalajs.dom.html
import shipreq.webapp.base.data.{FilterDead, ShowDead}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style

final class FilterDeadButtonObs($: DomZipperJs) {

  val button: html.Button =
    $("button").domAs[html.Button]

  val value: FilterDead =
    ShowDead when button.classList.contains("red")

  def toggle(): Unit =
    Simulate click button
}

object FilterDeadButtonObs {
  def needIn($: DomZipperJs): FilterDeadButtonObs =
    new FilterDeadButtonObs($(Style.savedViews.filterDeadButtonContainer.selector))
}
