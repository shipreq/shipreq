package com.beardedlogic.usecase
package lib
package field

import net.liftweb.util.Helpers._
import model._
import TextField._
import TypeTags._
import FieldValue.FieldValueData

/**
 * Text field definition.
 *
 * @param title Name/title of the field. E.g. "Pre-Conditions"
 */
case class TextFieldDef(title: String) extends FieldDef[String @@ NormalisedRefs] {

  override def newFieldInstance(ucCtx: UseCaseCtx, fieldKey: FieldKey) = new TextField(this, ucCtx, fieldKey)

  override def fieldKeyType = FieldKeyType.Text
  override def fieldKeyData = Some(title)

  override def stateLoader(fieldKey: FieldKey) = new StateLoader(fieldKey)
}

object TextField {

  import Fields.Template

  val TextTemplate = Template("template-text")

  class StateLoader(val fieldKey: FieldKey) extends FieldStateLoader[String @@ NormalisedRefs] {
    override def load(loadCtx: FieldLoadCtx, saveCtx: MutableFieldSaveCtx) =
      loadCtx.fieldValues.get(fieldKey).map(_.fieldData).flatten.getOrElse("").hasNormalisedRefs
  }
}

/**
 * A single, stateful text field instance.
 *
 * @param fd Identity of this text field.
 */
class TextField(val fd: TextFieldDef, override val ucCtx: UseCaseCtx, override val fieldKey: FieldKey)
  extends Field[String @@ NormalisedRefs] {

  val value = new SmartText(ucCtx.msgCentre, ucCtx.stepLabelMapProvider)

  override def init() {
    value.init()
  }

  override def render = renderExpr(TextTemplate)

  def renderExpr = (
    "th *" #> fd.title
      & "textarea" #> value.renderTextarea
    )

  override def setState(newState: String @@ NormalisedRefs): () => Unit = {
    () => value.setTextFromLoad(newState, ucCtx.savedSteps)
  }

  override def save_? : Boolean = value.text.nonEmpty

  override def presave(
    lastSave: Option[(FieldSaveCtx, String @@ NormalisedRefs)],
    saveCtx: MutableFieldSaveCtx,
    dao: DAO): Boolean = {

    value.recalcTextWithNormalisedRefs(ucCtx.savedSteps.ba)
    lastSave match {
      case None                    => true
      case Some((_, previousText)) => previousText != value.textWithNormalisedRefs
    }
  }

  override def save(combinedSaveCtx: FieldSaveCtx, newSaveCtx: FieldSaveCtx, dao: DAO): (FieldValueData, String @@ NormalisedRefs) =
    (Some(value.textWithNormalisedRefs), value.textWithNormalisedRefs)
}