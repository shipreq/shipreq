package shipreq.webapp.client.app.ui
package reqtable

import scalacss.ScalaCssReact._
import shipreq.webapp.client.lib.ui.TextEditor
import Style.{reqtable => *}

package object edit {

  def textSeqEditor[A](name: String, splitFn: String => Stream[String], textEditor: TextEditor = TextEditor.Input): TextSeqEditor[A] =
    new TextSeqEditor(name, splitFn, textEditor, *.cellEditor(_), *.cellEditorErrMsg)
}
