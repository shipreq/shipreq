package shipreq.webapp.server.feature.validation

import japgolly.microlibs.nonempty.NonEmptyVector
import scala.xml.{NodeSeq, Text}

import shipreq.webapp.base.vali2._

object InvalidityHtml {

  def simple(i: Simple.Invalidity): NodeSeq =
    lines(Simple.Invalidity.toLines(i))

  def composite(i: Composite.Invalidity): NodeSeq =
    lines(Composite.Invalidity.toLines(i))

  def lines(i: NonEmptyVector[String]): NodeSeq =
    if (i.tail.isEmpty)
      Text(i.head)
    else
      <ul>{i.iterator.map(t => <li>{t}</li> : NodeSeq).reduce(_ ++ _)}</ul>

}
