package shipreq.webapp.client.project.test

import shipreq.webapp.base.test.TestState._
import japgolly.scalajs.react.test._

object CommonObs {

  final class Dropdown($: DomZipperJs) {
    val hasError = $.domAsHtml.classList.contains("error")

    val selected = $(".text").innerText.trim

    def select(name: String): Unit = {
      val doms = $(".menu").collect0n(".item").filter(_.innerText.trim == name).doms
      if (doms.length == 1)
        Simulate.click(doms.head)
      else
        throw new RuntimeException(doms.map(_.innerText).mkString("Multiple candidates: ", ", ", ""))
    }
  }

}
