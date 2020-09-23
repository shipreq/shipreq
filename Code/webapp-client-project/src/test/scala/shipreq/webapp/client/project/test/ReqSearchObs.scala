package shipreq.webapp.client.project.test

import japgolly.scalajs.react.test._
import org.scalajs.dom.document
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.Style

final class ReqSearchObs($: DomZipperJs) {
  import ReqSearchObs._

  private val inputDom = $("input").domAsInput

  val query = inputDom.value

  def focus(): Unit = inputDom.focus()
  def blur(): Unit = inputDom.blur()
  def setQuery(q: String): Unit = SimEvent.Change(q).simulate(inputDom)

  val results: Option[Results] =
    $.collect01(".ui.popup.visible").map(new Results(_))

  val focused: Option[Int] =
    results.flatMap(_.focused)
}

object ReqSearchObs {

  final class Results($: DomZipperJs) {
    def noResults = items.isEmpty

    val items: Vector[Result] =
      $.collect0n(Style.widgets.reqSearch.result.selector).map(new Result(_))

    val focused: Option[Int] =
      items.iterator.zipWithIndex.find(_._1.focused).map(_._2)
  }

  final class Result($: DomZipperJs) {
    private def linkDom = $("a").dom

    val text    = $.innerText
    val pubid   = $(Style.widgets.reqSearch.resultPubid.selector).innerText.stripSuffix(":")
    val focused = document.activeElement == $.dom

    def click() = Simulate.click(linkDom)
  }

  // ===================================================================================================================

  final class TestDsl[R, O, S](val dsl: Dsl[Id, R, O, S, String])
                              (getObs: O => ReqSearchObs) {

    private implicit def autoObs(o: O): ReqSearchObs =
      getObs(o)

    val query =
      dsl.focus("Req search query").value(_.obs.query)

    val focusedResult =
      dsl.focus("Req search focused result").option(_.obs.focused)

    val resultsAreVisible =
      dsl.focus("Req search results are visible").value(_.obs.results.isDefined)

    val resultPubids =
      dsl.focus("Req search results").collection(_.obs.results.fold(Vector.empty[String])(_.items.map(_.pubid)))

    val focusInput: dsl.Actions =
      dsl.action("Focus req search")(_.obs.focus())

    val blurInput: dsl.Actions =
      dsl.action("Un-focus req search")(_.obs.blur())

    def setQuery(q: String): dsl.Actions =
      focusInput >> dsl.action("Set req search query to: " + JSON.stringify(q))(_.obs.setQuery(q))

    def clickResult(idx: Int): dsl.Actions =
      dsl.action(s"Click req search result #$idx")(_.obs.results.get.items(idx).click())
  }

}
