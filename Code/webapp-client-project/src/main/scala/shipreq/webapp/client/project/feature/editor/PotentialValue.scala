package shipreq.webapp.client.project.feature.editor

import japgolly.scalajs.react.Reusability
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.feature.clipboard.{ClipboardCodec, ClipboardData}

/** Data that might be an acceptable value for an editor. */
sealed trait PotentialValue

object PotentialValue {
  final case class Clipboard(value: ClipboardData) extends PotentialValue
  final case class Text     (value: String)        extends PotentialValue
  case object Emptiness                            extends PotentialValue

  implicit def univEq     : UnivEq     [PotentialValue] = UnivEq.derive
  implicit def reusability: Reusability[PotentialValue] = Reusability.derive
}

// =====================================================================================================================

private[editor] sealed trait SetValueDecision

private[editor] object SetValueDecision {
  case object Ignore         extends SetValueDecision
  case object Replace        extends SetValueDecision
  case object OpenAndReplace extends SetValueDecision

  def apply(editor: Feature.Read.ForAnyEditor): SetValueDecision =
    if (editor.isOpen)
      editor.async match {
        case None                                      => SetValueDecision.Replace
        case Some(AsyncFeature.Status.Failed(_, _, _)) => SetValueDecision.Replace
        case Some(AsyncFeature.Status.InProgress)      => SetValueDecision.Ignore
      }
    else
      SetValueDecision.OpenAndReplace
}

// =====================================================================================================================

final case class PotentialValueAcceptor[+A](accept: PotentialValue => Option[A])

object PotentialValueAcceptor {
  val rejectAll = apply(_ => None)

  def correct(f: String => String): PotentialValueAcceptor[String] = {
    val cc = ClipboardCodec.string.correct(f)
    apply {
      case PotentialValue.Clipboard(c) => cc.read(c)
      case PotentialValue.Text(t)      => Some(f(t))
      case PotentialValue.Emptiness    => Some(f(""))
    }
  }
}
