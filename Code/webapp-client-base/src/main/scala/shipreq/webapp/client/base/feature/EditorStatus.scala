package shipreq.webapp.client.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.validation.ValidatorU
import EditorStatus._

/** Editors in ShipReq can be in a variety of states:
  *
  * - [[Ignore]]. Ignore the value of the editor. No error. No action.
  *     Example: blank text on "create project" editor.
  *     Example: edit value same as saved value, no change.
  *
  * - [[Valid]]. Edit value is valid and can be committed.
  *
  * - [[Invalid]]. Edit value is invalid.
  *
  * - [[InTransit]]. An action has been sent to the server, now awaiting response.
  *
  * - [[AsyncError]]. An error occurred attempting to commit.
  */
sealed abstract class EditorStatus {

  final def isAsync: Boolean =
    this match {
      case _: Async => true
      case _: Sync  => false
    }

  final def getCommit: Option[Callback] =
    this match {
      case Valid(commit)        => Some(commit)
      case AsyncError(_, retry) => Some(retry)
      case Invalid(_)
         | Ignore
         | InTransit            => None
    }

  final def getError: Option[TagMod] =
    this match {
      case Valid(_)
         | Ignore
         | InTransit        => None
      case Invalid(e)       => Some(e)
      case AsyncError(e, _) => Some(e)
    }
}

object EditorStatus {
  sealed abstract class Sync  extends EditorStatus
  sealed abstract class Async extends EditorStatus

  case object Ignore                                   extends Sync
  case class  Valid(commit: Callback)                  extends Sync
  case class  Invalid(err: TagMod)                     extends Sync
  case object InTransit                                extends Async
  case class  AsyncError(err: TagMod, retry: Callback) extends Async

  def ignoreOrValidate[I, C, V](v: ValidatorU[I, C, V])(i: I, ignore: C => Boolean, commit: V => Callback): Sync = {
    val corrected = v.correctedU(i)
    if (ignore(corrected.value))
      Ignore
    else
      v.validateU(corrected) match {
        case scalaz.Success(ok)  => EditorStatus.Valid(commit(ok))
        case scalaz.Failure(err) => EditorStatus.Invalid(err.toText)
      }
  }

  def validate[I, C, V](v: ValidatorU[I, C, V])(i: I, commit: V => Callback): Sync =
    ignoreOrValidate(v)(i, _ => false, commit)

  private def maybeAsync[A](a: AsyncActionFeature.D0.State[A])(implicit f: A => TagMod): Option[Async] =
    a.map {
      case AsyncActionFeature.Locked       => InTransit
      case x: AsyncActionFeature.Failed[A] => AsyncError(f(x.failure), x.retry)
    }

  def async[A, I](asyncState  : AsyncActionFeature.D0.State[A],
                  asyncFeature: AsyncActionFeature.D0.Feature[A])
                 (updateValue : I => Callback,
                  syncState   : => Sync)
                 (implicit f: A => TagMod): (I => Callback, EditorStatus) = {

    val status = maybeAsync(asyncState) getOrElse syncState

    val updateValue2 = if (status.isAsync)
      updateValue.andThen(_ >> asyncFeature.clearError(asyncState))
    else
      updateValue

    (updateValue2, status)
  }

}
