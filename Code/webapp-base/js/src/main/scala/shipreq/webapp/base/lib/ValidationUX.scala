package shipreq.webapp.base.lib

import japgolly.univeq.UnivEq
import scalaz.\/
import shipreq.base.util

/** Description of the user experience regarding validation of data that they've supplied,
  * as they're supplying it.
  */
sealed trait ValidationUX {
  import ValidationUX.Outcome

  def outcome[E](error: => Option[E]): Outcome[E]

  final def outcomeD[E](validated: => (E \/ Any)): Outcome[E] =
    outcome(validated.fold(Some(_), _ => None))
}

object ValidationUX {

  /** Users receive no indication that their data is valid or invalid. */
  case object Off extends ValidationUX {
    override def outcome[E](error: => Option[E]) =
      Outcome.Valid
  }

  /** Subject is highlighted when invalid, usually just coloured red */
  case object Highlight extends ValidationUX {
    override def outcome[E](error: => Option[E]) =
      if (error.isEmpty) Outcome.Valid else Outcome.Invalid(None)
  }

  /** Subject is highlighted when invalid, and the reason for invalidity is explained. */
  case object Full extends ValidationUX {
    override def outcome[E](error: => Option[E]) = {
      val e = error
      if (e.isEmpty) Outcome.Valid else Outcome.Invalid(e)
    }
  }

  implicit def univEq: UnivEq[ValidationUX] = UnivEq.derive

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed trait Outcome[+E] {
    def map[A](f: E => A): Outcome[A]
  }

  object Outcome {
    case object Valid extends Outcome[Nothing] {
      override def map[A](f: Nothing => A) = this
    }

    final case class Invalid[+E](error: Option[E]) extends Outcome[E] {
      override def map[A](f: E => A) = Invalid(error map f)
    }

    def apply(v: util.Validity) =
      v match {
        case util.Valid   => Valid
        case util.Invalid => Invalid(None)
      }
  }
}