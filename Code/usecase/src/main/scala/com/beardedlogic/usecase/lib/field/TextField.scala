package com.beardedlogic.usecase
package lib
package field

import net.liftweb.util.Helpers._
import model._
import TextField._


/**
 * Text field definition.
 *
 * @param title Name/title of the field. E.g. "Pre-Conditions"
 */
case class TextFieldDef(title: String) extends FieldDef[String] {

  override def newFieldInstance(ucCtx: UseCaseCtx, fieldKey: FieldKey) = new TextField(this, ucCtx, fieldKey)

  override def fieldKeyType = FieldKeyType.Text
  override def fieldKeyData = Some(title)

  override def stateLoader(fieldKey: FieldKey) = new StateLoader(fieldKey)
}

object TextField {

  import Fields.Template

  val TextTemplate = Template("template-text")

  class StateLoader(val fieldKey: FieldKey) extends FieldStateLoader[String] {
    override def load(ctx: FieldLoadCtx) =
      ctx.fieldValues.get(fieldKey.valueId).map(_.fieldData).flatten.getOrElse("")
  }

  object StateSaver extends FieldStateSaver[String] {
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
class TextField(val fd: TextFieldDef, override val ucCtx: UseCaseCtx, override val fieldKey: FieldKey)
  extends Field[String] {

  val value = new SmartText(ucCtx.msgCentre, ucCtx.stepLabelMapProvider)

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
  override def stateSaver = StateSaver
}