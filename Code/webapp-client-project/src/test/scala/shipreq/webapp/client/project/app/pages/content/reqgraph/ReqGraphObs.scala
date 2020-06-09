package shipreq.webapp.client.project.app.pages.content.reqgraph

import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style.{reqgraphPage => *}
import shipreq.webapp.client.project.feature.savedview._
import shipreq.webapp.client.project.test.CommonObs.Dropdown

final class ReqGraphObs($: DomZipperJs) {

//  println()
//  println($.outerHTML)
//  println()

  private val controlsRow2 = $(">div", 2 of 3)

  val savedViews = SavedViewManagerObs.needIn($)
  val filterDead = FilterDeadButtonObs.needIn($)
  val filter     = FilterEditorObs.needIn($)

  val colours = new Dropdown(controlsRow2(*.configColoursEditor.selector).child(".ui.dropdown"))

}