package com.beardedlogic.usecase.lib
package field

import net.liftweb.util.Helpers._

object TextField {
  import Fields.Template

  val TextTemplate = Template("template-text")
}

case class TextFieldDef(
  title: String,
  hint: Option[String]) extends FieldDef {

  def newFieldInstance = new TextField(this)
}

class TextField(fd: TextFieldDef) extends Field {
  import TextField._

  val renderExpr = (
    "th *" #> fd.title
    & "textarea *" #> ""
  )

  def render = renderExpr(TextTemplate)
}