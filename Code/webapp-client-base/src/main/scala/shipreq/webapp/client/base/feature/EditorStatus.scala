package shipreq.webapp.client.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.validation.{ValidationResult, ValidatorU}
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
  *
  *
  * Usage
  * =====
  *
  * - Inject [[getCommit]] into keyboard and/or button events.
  * - Wrap the editor onChange callback in [[wrapEdit]].
  * - Pattern match and render as needed.
  */
sealed abstract class EditorStatus {

  final def wrapEdit(edit: Callback): Callback =
    this match {
      case Ignore
         | Valid(_)
         | Invalid(_)    => edit
      case a: AsyncError => edit >> a.clearAsync
      case InTransit     => Callback.empty
    }

  final def getCommit: Option[Callback] =
    this match {
      case Valid(commit) => Some(commit)
      case a: AsyncError => Some(a.retry)
      case Invalid(_)
         | Ignore
         | InTransit     => None
    }

  final def getError: Option[TagMod] =
    this match {
      case Valid(_)
         | Ignore
         | InTransit           => None
      case Invalid(e)          => Some(e)
      case AsyncError(e, _, _) => Some(e)
    }
}

object EditorStatus {
  sealed abstract class Sync  extends EditorStatus
  case object Ignore extends Sync
  case class Valid(commit: Callback) extends Sync
  case class Invalid(err: TagMod) extends Sync

  sealed abstract class Async extends EditorStatus
  case object InTransit extends Async
  case class AsyncError(err: TagMod, retry: Callback, clearAsync: Callback) extends Async

  // ===================================================================================================================

  def ignoreOrValidate[I, C, V](v: ValidatorU[I, C, V])(i: I, ignore: C => Boolean, commit: V => Callback): Sync = {
    val corrected = v.correctedU(i)
    if (ignore(corrected.value))
      Ignore
    else
      validationResult(v.validateU(corrected))(commit)
  }

  def validate[I, C, V](v: ValidatorU[I, C, V])(i: I, commit: V => Callback): Sync =
    ignoreOrValidate(v)(i, _ => false, commit)

  def validationResult[V](vr: ValidationResult[V])(commit: V => Callback): Sync =
    vr match {
      case scalaz.Success(v)   => Valid(commit(v))
      case scalaz.Failure(err) => Invalid(err.toText)
    }

  def async[A, I](as: AsyncActionFeature.D0.State[A],
                  af: AsyncActionFeature.D0.Feature[A])
                 (implicit f: A => TagMod): Option[Async] =
    as map {
      case AsyncActionFeature.Locked       => InTransit
      case x: AsyncActionFeature.Failed[A] => AsyncError(f(x.failure), retry = x.retry, clearAsync = af.clearError(as))
    }
}
