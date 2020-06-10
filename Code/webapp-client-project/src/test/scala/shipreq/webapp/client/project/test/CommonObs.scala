package shipreq.webapp.client.project.test

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.ui.semantic.JQuery

object CommonObs {

  final class Input($: DomZipperJs) {
    val dom = $.domAs[html.Input]
    val value = dom.value
    def setValue(v: String) = SimEvent.Change(v).simulate(dom)
  }

  final class TextArea($: DomZipperJs) {
    val dom = $.domAs[html.TextArea]
    val value = dom.value
    def setValue(v: String) = SimEvent.Change(v).simulate(dom)
  }

  final class Link($: DomZipperJs) {
    val dom = $.domAs[html.Anchor]
    val label = $.innerText.trim
    def click() = Simulate.click(dom)
  }

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

        if (itemDom.onclick ne null)
          Simulate.click(itemDom)
        else
          Option(itemDom.getAttribute("data-value")).filter(_.nonEmpty) match {
            case Some(value) => JQuery(dom).dropdown("set selected", value)
            case None        => throw new RuntimeException(s"Don't know how to select item in:\n\n${dom.outerHTML}")
          }
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
