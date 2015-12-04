package shipreq.webapp.client.lib.ui.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import scalaz.Need
import shipreq.base.util.{Valid, Validity}
import shipreq.webapp.base.validation._
import shipreq.webapp.client.app.ui.Style.{reqtable => *}

// TODO not really
import shipreq.webapp.client.lib.ui.KeyHandlers

sealed abstract class EditValidationFeature[+A] {
  def validated: Option[A]

  def renderFailure: Option[ReactElement]

//  final def validatedN[B](f: A => B): Option[Need[B]] =
//    validated.map(a => Need(f(a)))

  final def validity: Validity =
    Valid <~ validated.isDefined

  final def commitByKeyboard(f: A => Callback, singleLine: Boolean): KeyHandlers =
    validated match {
      case Some(a) => KeyHandlers.commit(Some(f(a)), singleLine)
      case None    => KeyHandlers.empty
    }
}

object EditValidationFeature {
  @inline def apply[F[_], A](result: F[A])(implicit h: Handler[F]): EditValidationFeature[A] =
    h(result)

  private class WhenValid[+A](a: A) extends EditValidationFeature[A] {
    override val validated     = Some(a)
    override def renderFailure = None
  }

  private class WhenInvalid(rf: () => ReactElement) extends EditValidationFeature[Nothing] {
    override def validated     = None
    override def renderFailure = Some(rf())
  }

  trait Handler[F[_]] {
    def apply[A](result: F[A]): EditValidationFeature[A]
  }

  import scalaz._

  private def errorTag: ReactTag =
    <.div(*.cellEditorErrMsg)

  implicit val HandleValidationResult: Handler[ValidationResult] =
    new Handler[ValidationResult] {
      override def apply[A](result: ValidationResult[A]) =
        result match {
          case Success(a) => new WhenValid(a)
          case Failure(f) => new WhenInvalid(() => errorTag(f.toText)) // TODO Do better
        }
    }

//  type StringOr[A] = String \/ A
//  implicit val HandleStringOrA: Handler[StringOr] =
//    new Handler[StringOr] {
//      override def apply[A](result: StringOr[A]) =
//        result match {
//          case \/-(a) => new WhenValid(a)
//          case -\/(f) => new WhenInvalid(() => <.div(f))
//        }
//    }
}
