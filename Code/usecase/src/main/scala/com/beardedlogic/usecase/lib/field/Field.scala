package com.beardedlogic.usecase.lib
package field

import scala.xml.NodeSeq

trait FieldDef {
  def newFieldInstance: Field
}

trait Field {
  def render: NodeSeq
}
