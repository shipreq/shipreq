package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.{ErrorMsg, PotentialChange}
import shipreq.webapp.base.feature.EditorStatus._
import shipreq.webapp.base.validation.Simple._

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
      case Valid(commit) => commit
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
  case object Ignore                          extends Sync
  case class  Valid(commit: Option[Callback]) extends Sync
  case class  Invalid(err: TagMod)            extends Sync

  sealed abstract class Async extends EditorStatus
  case object InTransit                                                      extends Async
  case class  AsyncError(err: TagMod, retry: Callback, clearAsync: Callback) extends Async

  // ===================================================================================================================

  def ignoreOrValidate[I, C, V](v: Validator[I, C, V])(i: I, ignore: C => Boolean, commit: V => Option[Callback]): Sync = {
    val corrected = v.corrector(i)
    if (ignore(corrected))
      Ignore
    else
      fromValidation(v.auditor(corrected))(commit)
  }

  def validate[I, C, V](v: Validator[I, C, V])(i: I, commit: V => Option[Callback]): Sync =
    ignoreOrValidate(v)(i, _ => false, commit)

  def fromValidation[V](vr: Invalidity \/ V)(commit: V => Option[Callback]): Sync =
    vr match {
      case \/-(v) => Valid(commit(v))
      case -\/(e) => Invalid(Invalidity.toText(e))
    }

  def potentialChange[E, A](vu: PotentialChange[E, A])
                           (commit: A => Option[Callback], unchanged: Option[Callback])
                           (implicit fmtErr: E => TagMod): Sync =
    vu match {
      case PotentialChange.Success(a) => Valid(commit(a))
      case PotentialChange.Unchanged  => Valid(unchanged)
      case PotentialChange.Failure(e) => Invalid(fmtErr(e))
    }

  def fromValidatedChange[A](vu: PotentialChange[Invalidity, A])
                            (commit: A => Option[Callback], unchanged: Option[Callback]): Sync =
    potentialChange(vu)(commit, unchanged)(Invalidity.toText)

  def async[I](a: AsyncFeature.Read.D0[ErrorMsg]): Option[Async] =
    a map {
      case AsyncFeature.Status.InProgress          => InTransit
      case x: AsyncFeature.Status.Failed[ErrorMsg] => AsyncError(x.failure.value, retry = x.retry, clearAsync = x.cancel)
    }
}
