package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import scalaz.Equal
import scalaz.effect.IO
import shipreq.base.util.SetDiff
import shipreq.webapp.client.lib.TIO
import shipreq.webapp.client.lib.ui.UI

/**
 * An abstraction of a editor with the following properties:
 *
 * - locks itself when a remote request is sent.
 * - facilitates retry when a remote request fails.
 * - can avoid no-op updates.
 */
object RemoteDataEditor {

  sealed trait Status
  case object Editing extends Status
  case object Locked extends Status
  case class Failed(retry: () => IO[Unit], resumeEdit: () => IO[Unit]) extends Status

  case class StateFor[+A](value: A, status: Status, renderFn: () => ReactElement) {
    @inline def render: ReactElement =
      renderFn()
  }

  type State = StateFor[Any]

  type OpState        = Option[State]
  type OpStateFor[+A] = Option[StateFor[A]]

  type SetState        = State => IO[Unit]
  type SetStateFor[-A] = StateFor[A] => IO[Unit]

  type SetOpState        = OpState => IO[Unit]
  type SetOpStateFor[-A] = OpStateFor[A] => IO[Unit]

  class Callbacks(
    val abort    : TIO.Abort,
    val lock     : IO[Unit],
    val succeeded: TIO.Success,
    val failed   : TIO.Failure)

  type OnCommit = Callbacks => IO[Unit]
  type CommitFn = OnCommit => TIO.Commit

  // ===================================================================================================================

  private def core[SF[_], S, A](initial   : A,
                                convInput : S => A,
                                setSelf   : SF[A] => IO[Unit],
                                makeSF    : StateFor[A] => SF[A],
                                abortFn   : ((A, Status) => SF[A]) => TIO.Abort,
                                successFn : TIO.Abort => TIO.Success,
                                renderEdit: (A, S => IO[Unit], TIO.Abort, CommitFn) => ReactElement,
                                renderLock: A => ReactElement,
                                renderFail: (A, Failed) => ReactElement): SF[A] = {

    lazy val abort = abortFn(state)
    lazy val success = successFn(abort)

    def commit(a: A): CommitFn =
      onCommit => {
        def onFailure: TIO.Failure = TIO.Failure.lazily {
          def ff = Failed(() => onCommit(callbacks), () => setSelf(editState(a)))
          setSelf(state(a, ff))
        }

        def callbacks: Callbacks =
          new Callbacks(
            abort,
            setSelf(state(a, Locked)),
            success,
            onFailure)

        TIO.Commit(onCommit(callbacks))
      }


    def state(a: A, status: Status): SF[A] = {
      def render: ReactElement =
        status match {
          case Editing   => renderEdit(a, recvEdit, abort, commit(a))
          case Locked    => renderLock(a)
          case f: Failed => renderFail(a, f)
        }
      makeSF(StateFor(a, status, () => render))
    }

    def recvEdit: S => IO[Unit] =
      s => setSelf(editState(convInput(s)))

    def editState(a: A): SF[A] =
      state(a, Editing)

    editState(initial)
  }

  def default[S, A](initial   : A,
                    convInput : S => A,
                    setSelf   : SetStateFor[A],
                    renderEdit: (A, S => IO[Unit], TIO.Abort, CommitFn) => ReactElement): StateFor[A] =
    core[StateFor, S, A](
      initial, convInput, setSelf, s => s,
      f => TIO.Abort(setSelf(f(initial, Editing))),
      TIO.Success(_),
      renderEdit, defaultRenderLock, defaultRenderFail)

  def opDefault[S, A](initial   : A,
                      convInput : S => A,
                      setSelf   : SetOpStateFor[A],
                      renderEdit: (A, S => IO[Unit], TIO.Abort, CommitFn) => ReactElement): OpStateFor[A] =
    core[OpStateFor, S, A](
      initial, convInput, setSelf, Some(_),
      _ => TIO.Abort(setSelf(None)),
      TIO.Success(_),
      renderEdit, defaultRenderLock, defaultRenderFail)

  // ===================================================================================================================

  val defaultRenderLock: Any => ReactElement =
    _ => UI.spinner

  val defaultRenderFail: (Any, Failed) => ReactElement =
    (_, f) => renderRetry(f.retry(), f.resumeEdit())

  private def renderRetry(retryFn: => IO[Unit], resumeFn: => IO[Unit]) =
    <.div(
      "Network error occurred.",
      <.button("Retry", ^.onClick ~~> retryFn), // English
      <.button("OK", ^.onClick ~~> resumeFn)) // English

  // ===================================================================================================================

  case class CommitFilter[A](f: A => OnCommit) extends AnyVal {
    def cmapo[B](g: B => Option[A]): CommitFilter[B] =
      CommitFilter(b =>
        g(b) match {
          case Some(a) => f(a)
          case None    => _.abort.io
        }
      )

    def cmap[B](f: B => A): CommitFilter[B] =
      cmapo(b => Some(f(b)))

    def ignore(f: A => Boolean): CommitFilter[A] =
      cmapo(a => if (f(a)) None else Some(a))

    def ignoreIfEqual(initial: A)(implicit e: Equal[A]): CommitFilter[A] =
      ignore(e.equal(initial, _))

    def cmapToInitial[B: Equal](initial: B)(f: B => A): CommitFilter[B] =
      cmap(f).ignoreIfEqual(initial)

    def setDiff[B](f: SetDiff[B] => A): CommitFilter[SetDiff[B]] =
      cmap(f).ignore(_.isEmpty)

    @inline def apply(a: A): OnCommit =
      f(a)
  }
  
}
