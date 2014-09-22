package utily

import org.scalajs.dom.{HTMLInputElement, console}
import org.scalajs.dom.extensions.KeyCode
import scala.runtime.AbstractFunction3
import scalaz.effect.IO
import scalaz.Scalaz.Id
import scalaz._
import scalaz.std.option._
import scalaz.syntax.bind._
import scalaz.syntax.foldable._
import monocle._

import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui._
import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._
import ScalazReact._


/**
 * S = State. Where the subject data lives.
 * W = Row ID.
 */
object EditorStuff {


  // ===================================================================================================================
  // Validation


  object KeyValidator extends Validator[String, String, String] {
    override def liveCorrect = _.toUpperCase()
    override def correct = _.trim.toUpperCase()
    override def validate = {
      case "" => -\/("It's blank!")
      case s if !s.matches("^[A-Z]+$") => -\/("One word, A-Z only.")
      case s => \/-(s)
    }
    override def c2i = identity
  }

  object MnemonicValidator extends Validator[String, String, String] {
    override def liveCorrect = _.toUpperCase().replaceAll("[^A-Z]+","").replaceAll("^(.{6}).+","$1")
    override def correct = _.trim.toUpperCase()
    override def validate = {
      case "" => -\/("It's blank!")
      case s if !s.matches("^[A-Z]{1,6}$") => -\/("A-Z only, 1-6 letters.")
      case s => \/-(s)
    }
    override def c2i = identity
  }

  object ReqNameValidator extends Validator[String, String, String] {
    override def liveCorrect = identity
    override def correct = _.trim
    override def validate = {
      case "" => -\/("It's blank!")
      case s => \/-(s)
    }
    override def c2i = identity
  }

  object DescValidator extends Validator[String, Option[String], Option[String]] {
    override def liveCorrect = identity
    override def c2i = _ getOrElse ""
    override def correct = s => {
      val j = s.trim
      if (j.isEmpty) None else Some(j)
    }
    override def validate = \/-(_)
  }

}
