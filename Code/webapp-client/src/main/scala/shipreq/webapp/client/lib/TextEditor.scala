package shipreq.webapp.client.lib

import japgolly.scalajs.react._, vdom.prefix_<^._
import org.scalajs.dom.html

sealed abstract class TextEditor {
  type Dom <: html.Element
  @inline final def asImplicit: TextEditor.OfType[Dom] = this

  def tag: ReactTagOf[Dom]
  def multiLine: Boolean
  def value(d: Dom): String
  def focus(d: Dom): Unit
  def select(d: Dom): Unit

  final def singleLine = !multiLine
}

object TextEditor {
  type OfType[D <: html.Element] = TextEditor {type Dom = D}

  implicit object Input extends TextEditor {
    override type Dom           = html.Input
    override def tag            = <.input.text
    override def multiLine      = false
    override def value (d: Dom) = d.value
    override def focus (d: Dom) = d.focus()
    override def select(d: Dom) = d.select()
  }

  implicit object TextArea extends TextEditor {
    override type Dom           = html.TextArea
    override def tag            = <.textarea
    override def multiLine      = true
    override def value (d: Dom) = d.value
    override def focus (d: Dom) = d.focus()
    override def select(d: Dom) = d.select()
  }
}
