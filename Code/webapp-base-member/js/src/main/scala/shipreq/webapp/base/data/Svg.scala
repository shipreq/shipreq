package shipreq.webapp.base.data

import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.jsfacade.HtmlReactParser

final case class Svg(content: String) {

  def trimmed: String =
    content.drop(content.indexOf("<svg")).trim

  lazy val vdom: VdomNode =
    HtmlReactParser.parse(trimmed)
}

object Svg {

  implicit def univEq: UnivEq[Svg] =
    UnivEq.derive

  implicit val reusability: Reusability[Svg] =
    Reusability.derive
}
