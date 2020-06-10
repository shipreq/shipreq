package shipreq.webapp.client.project.lib

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import shipreq.webapp.base.text._

sealed abstract class TextEditor {
  type Dom <: html.Element
  @inline final def asImplicit: TextEditor.OfType[Dom] = this

  def tag: VdomTagOf[Dom]
  def lineCardinality: LineCardinality
  def value(d: Dom): String
  def focus(d: Dom): Unit
  def select(d: Dom): Unit
}

object TextEditor {
  type OfType[D <: html.Element] = TextEditor {type Dom = D}

  implicit object Input extends TextEditor {
    override type Dom            = html.Input
    override def tag             = <.input.text
    override def lineCardinality = SingleLine
    override def value (d: Dom)  = d.value
    override def focus (d: Dom)  = d.focus()
    override def select(d: Dom)  = d.select()
  }

  implicit object TextArea extends TextEditor {
    override type Dom            = html.TextArea
    override def tag             = <.textarea
    override def lineCardinality = MultiLine
    override def value (d: Dom)  = d.value
    override def focus (d: Dom)  = d.focus()
    override def select(d: Dom)  = d.select()
  }
}
