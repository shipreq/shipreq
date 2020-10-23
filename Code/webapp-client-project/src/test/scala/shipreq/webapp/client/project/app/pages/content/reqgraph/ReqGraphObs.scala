package shipreq.webapp.client.project.app.pages.content.reqgraph

import shipreq.webapp.base.test.CommonObs.Dropdown
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style.{reqgraphPage => *}
import shipreq.webapp.client.project.feature.savedview._
import shipreq.webapp.client.project.test.{ImpGraphObs, TestGlobal}

final class ReqGraphObs($: DomZipperJs, val global: TestGlobal.Obs) {

//  println()
//  println($.outerHTML)
//  println()

  private lazy val controlsRow2 = $(">div", 2 of 3)

  lazy val savedViews = SavedViewManagerObs.needIn($)
  lazy val filterDead = FilterDeadButtonObs.needIn($)
  lazy val filter     = FilterEditorObs.needIn($)

  lazy val colours = new Dropdown(controlsRow2(*.configColoursEditor.selector).child(".ui.dropdown"))

  // not lazy because invariants exist
  val graph = ImpGraphObs.find($)
}