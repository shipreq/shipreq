package shipreq.webapp.client.project.feature.savedview

import org.scalajs.dom.html
import shipreq.base.util.{Invalid, Validity}
import shipreq.webapp.base.test.TestState._

final class FilterEditorObs(inputZipper: DomZipperJs) {

  val input: html.Input =
    inputZipper.domAs[html.Input]

  val value: String =
    input.value

  val validity: Validity =
    Invalid when input.parentNode.asInstanceOf[html.Element].classList.contains("error")
}

object FilterEditorObs {
  def needIn($: DomZipperJs): FilterEditorObs =
    new FilterEditorObs($("input[placeholder='Filter...']"))
}
