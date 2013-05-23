package com.beardedlogic.usecase
package lib
package field

import net.liftweb.util.Helpers._
import model.FieldKeyType

/**
 * Text field definition.
 *
 * @param title Name/title of the field. E.g. "Pre-Conditions"
 */
case class TextFieldDef(title: String) extends FieldDef {

  override def newFieldInstance(state: UCEditorState) = new TextField(this, state)

  override def fieldKeyType = FieldKeyType.Text
  override def fieldKeyData = Some(title)
}

object TextField {
  import Fields.Template

  val TextTemplate = Template("template-text")
}

/**
 * A single, stateful text field instance.
 *
 * @param fd Identity of this text field.
 */
class TextField(val fd: TextFieldDef, val state: UCEditorState) extends Field {
  import TextField._

  val value = new SmartText(state.msgCentre, state.stepLabelMapProvider)

  override def init() {
    value.init()
  }

  override def render = renderExpr(TextTemplate)

  def renderExpr = (
    "th *" #> fd.title
    & "textarea" #> value.renderTextarea
  )
}