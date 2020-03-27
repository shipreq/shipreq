package shipreq.webapp.client.project.test

import shipreq.webapp.base.test.TestState._
import japgolly.scalajs.react.test._
import shipreq.webapp.base.ui.semantic.JQuery

object CommonObs {

  final class Dropdown($: DomZipperJs) {

    private val $$ = {
      val d = $.domAsHtml
      if (d.classList.contains("dropdown"))
        $
      else
        $(".ui.dropdown")
    }

    val dom = $$.domAsHtml

    val hasError = $.domAsHtml.classList.contains("error")

    val selected = $$.collect01(".text").innerTexts.map(_.trim)

    val items = $$.collect0n(".menu .item").map(new DropdownItem(_))

    def select(name: String): Unit = {
      val itemDoms = items.iterator.filter(_.text == name).map(_.dom).toVector
      if (itemDoms.length == 1) {
        val itemDom = itemDoms.head
        JQuery(dom).dropdown("set selected", itemDom.getAttribute("data-value"))
      } else
        throw new RuntimeException(itemDoms.map(_.innerText).mkString("Multiple candidates: ", ", ", ""))
    }
  }

  final class DropdownItem($: DomZipperJs) {
    val dom  = $.domAsHtml
    val text = dom.innerText.trim
  }

  final class DropdownButton($: DomZipperJs) {
    val dropdown      = new Dropdown($(".ui.dropdown"))
    val buttonDom     = $(".ui.button").domAsHtml
    def click(): Unit = Simulate click buttonDom
  }

  final class Message($: DomZipperJs) {
    val header = $(".header").innerText.trim

    lazy val body = {
      var t = $(".content").innerText
      if (t.startsWith(header))
        t = t.drop(header.length)
      t.trim
    }
  }
}
