package shipreq.webapp.client.app.ui
package reqtable

import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-, Failure, Success}
import shipreq.base.util.Validity
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.validation.ValidationResult
import shipreq.webapp.client.lib.ui.TextEditor
import Style.{reqtable => *}
import TextSeqEditor._

package object edit {

  val cellStyle: Validity => TagMod = *.cellEditor(_)
  def cellErrorMsgStyle: TagMod = *.cellEditorErrMsg

  def textSetEditor[A, B](name: String, splitFn: String => Stream[String], textEditor: TextEditor = TextEditor.Input): TextSeqEditor[A, B] =
    new TextSeqEditor(name, splitFn, textEditor, cellStyle, cellErrorMsgStyle)

  implicit def validatorInTextSeqEditor[A](v: ValidationResult[A]): ParseResult[A] =
    v match {
      case Success(s) => \/-(s)
      case Failure(f) => -\/(Some(f.toText))
    }

  def toSetWithoutValidation[A]: Vector[A] => ParseResult[Set[A]] =
    as => \/-(as.toSet)

  type UpdateContentOnCommit = RemoteDataEditor.CommitFilter[UpdateContentCmd]
}
