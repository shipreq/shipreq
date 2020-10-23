package shipreq.webapp.client.project.app.pages.content.reqgraph

import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.feature.savedview.SavedViewTestDsl
import shipreq.webapp.client.project.test._

object ReqGraphTestDsl {

  final case class Ref(global  : TestGlobal,
                       promptJs: TestPromptJs,
                       ww      : TestWebWorkerClient)

  val * = Dsl[Ref, ReqGraphObs, Unit]

  def invariants: *.Invariants =
    graph.invariants

  val global = new TestGlobal.TestDslWithObs(*)(_.global, _.global)

  val ww = new TestWebWorkerClient.TestDsl(*)(_.ww)

  val savedViews = SavedViewTestDsl(*)(_.savedViews, _.filterDead, _.filter, _.promptJs)

  val graph = new ImpGraphObs.TestDsl(*)(_.graph)

  val colours = *.focus("Colours").option(_.obs.colours.selected)

  val colourOptions = *.focus("Colour options").collection(_.obs.colours.items.map(_.text))

  def selectColours(name: String) =
    *.action("Select Colours: " + name)(_.obs.colours.select(name))
}
