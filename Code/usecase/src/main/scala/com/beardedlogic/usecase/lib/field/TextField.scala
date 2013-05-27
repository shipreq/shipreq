package com.beardedlogic.usecase
package lib
package field

import net.liftweb.util.Helpers._
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

  class StateDao(val fieldKey: FieldKey) extends FieldStateMiniDao[String] {
    override def load(ctx: FieldLoadCtx) =
      ctx.fieldValues.get(fieldKey.valueId).map(_.fieldData).flatten.getOrElse("")

    override def save_?(state: String): Boolean = state.nonEmpty
    override def presave(state: String, ctx: FieldSaveCtx) {}
    override def save(state: String, ctx: FieldSaveCtx) = Some(state)
    // TODO Change references
    // TODO References
  }
}

/**
 * A single, stateful text field instance.
 *
 * @param fd Identity of this text field.
 */
class TextField(val fd: TextFieldDef, override val uceState: UCEditorState, override val fieldKey: FieldKey)
  extends Field[String] {

  import TextField._

  val value = new SmartText(uceState.msgCentre, uceState.stepLabelMapProvider)

  override def init() {
    value.init()
  }

  override def render = renderExpr(TextTemplate)

  def renderExpr = (
    "th *" #> fd.title
      & "textarea" #> value.renderTextarea
    )

  override def state = value.text
  override def state_=(newState: String) = value.setTextFromUser(newState)
  override val stateDao = new StateDao(fieldKey)
}