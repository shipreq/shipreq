package com.beardedlogic.usecase.lib
package field

import net.liftweb.util.Helpers._

/**
 * Text field definition.
 *
 * @param title Name/title of the field. E.g. "Pre-Conditions"
 * @param hint Optional help text to display when the field is unpopulated.
 */
case class TextFieldDef(
                         title: String,
                         hint: Option[String]
                         ) extends FieldDef {

  override def newFieldInstance(state: UCEditorState) = new TextField(this, state)
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
class TextField(val fd: TextFieldDef, val state: UCEditorState) extends Field with Logger {
  import TextField._

  val renderExpr = (
    "th *" #> fd.title
    & "textarea *" #> ""
  )

  def render = renderExpr(TextTemplate)
}