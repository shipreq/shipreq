package shipreq.webapp.base.lib

import japgolly.scalajs.react.{CallbackTo, Reusability}

/** Abstraction over JS's `window.prompt`. */
trait PromptJs {
  def apply(message: String): CallbackTo[Option[String]]
  def apply(message: String, default: String): CallbackTo[Option[String]]
}

object PromptJs {

  val real: PromptJs =
    new PromptJs {
      override def apply(message: String): CallbackTo[Option[String]] =
        CallbackTo.prompt(message)

      override def apply(message: String, default: String): CallbackTo[Option[String]] =
        CallbackTo.prompt(message, default)
    }

  def const(answer: Option[String]): PromptJs =
    const(CallbackTo.pure(answer))

  def const(cb: CallbackTo[Option[String]]): PromptJs =
    new PromptJs {
      override def apply(message: String) = cb
      override def apply(message: String, default: String) = cb
    }

  implicit def reusability: Reusability[PromptJs] =
    Reusability.byRef
}