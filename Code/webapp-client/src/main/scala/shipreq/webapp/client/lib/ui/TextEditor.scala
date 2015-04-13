package shipreq.webapp.client.lib.ui

import org.scalajs.dom.html

abstract class TextEditor[_E <: html.Element] {
  final type E = _E
  def value(e: E): String
  def focus(e: E): Unit
  def select(e: E): Unit
}

object TextEditor {

  //@inline def apply[E <: html.Element](e: E)(implicit i: TextEditor[E]) = i

  implicit object Input extends TextEditor[html.Input] {
    override def value(e: E) = e.value
    override def focus(e: E) = e.focus()
    override def select(e: E) = e.select()
  }

  implicit object TextArea extends TextEditor[html.TextArea] {
    override def value(e: E) = e.value
    override def focus(e: E) = e.focus()
    override def select(e: E) = e.select()
  }
  
}
