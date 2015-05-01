package shipreq.webapp.client.app.ui
package reqtable

import scalacss.ScalaCssReact._
import Style.{reqtable => *}

package object edit {

  def textSeqEditor[A](name: String, splitFn: String => Stream[String]): TextSeqEditor[A] =
    new TextSeqEditor(name, splitFn, *.cellEditor(_), *.cellEditorErrMsg)
}
