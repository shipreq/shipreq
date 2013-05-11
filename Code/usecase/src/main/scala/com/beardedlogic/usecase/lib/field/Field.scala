package com.beardedlogic.usecase.lib
package field

import scala.xml.NodeSeq

trait FieldDef {
  def newFieldInstance(state: UCEditorState): Field
}

trait Field {

  val state: UCEditorState

  def render: NodeSeq
}
