package shipreq.webapp.client.project.app.pages.content.reqgraph

import shipreq.webapp.base.event.Event
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.feature.savedview.SavedViewTestDsl
import shipreq.webapp.client.project.test.{TestGlobal, TestPromptJs}

object ReqGraphTestDsl {

  final case class Ref(global: TestGlobal, promptJs: TestPromptJs)

  val * = Dsl[Ref, ReqGraphObs, Unit]

  val invariants = *.emptyInvariant

  val savedViews = SavedViewTestDsl(*)(_.savedViews, _.filterDead, _.filter, _.promptJs)

  val colours = *.focus("Colours").option(_.obs.colours.selected)

  val colourOptions = *.focus("Colour options").collection(_.obs.colours.items.map(_.text))

  def selectColours(name: String) =
    *.action("Select Colours: " + name)(_.obs.colours.select(name))

  def receiveExternalEvent(e: Event): *.Actions =
    *.action("Receive external event: " + e)(_.ref.global.applyTestEventsCB(e).void.runNow())
}
