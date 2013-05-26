package com.beardedlogic.usecase
package lib
package field

import net.liftweb.util.Helpers._
import scala.slick.session.Session
import model._
import FieldValue.FieldValueData

/**
 * Text field definition.
 *
 * @param title Name/title of the field. E.g. "Pre-Conditions"
 */
case class TextFieldDef(title: String) extends FieldDef {

  override def newFieldInstance(state: UCEditorState, fieldKey: FieldKey) = new TextField(this, state, fieldKey)

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
class TextField(val fd: TextFieldDef, override val state: UCEditorState, override val fieldKey: FieldKey) extends Field {
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

  override def save_? : Boolean = value.text.nonEmpty

  // TODO Change references
  // TODO References

  override def presave(ctx:FieldSaveCtx) {}

  override def save(ctx:FieldSaveCtx): FieldValueData = {
    Some(value.text)
  }
}