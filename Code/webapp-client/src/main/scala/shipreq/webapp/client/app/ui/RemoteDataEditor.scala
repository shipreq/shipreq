package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import shipreq.base.util.SetDiff
import shipreq.webapp.client.lib.ui.UI
import scalaz.Equal
import scalaz.effect.IO
import shipreq.webapp.client.lib.TIO

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

  type State = StateFor[Any]

  case class StateFor[+A](value: A, status: Status, renderFn: () => ReactElement) {
    @inline def render: ReactElement =
      renderFn()
  }

  class Callbacks(
    val abort    : TIO.Abort,
    val lock     : IO[Unit],
    val succeeded: TIO.Success,
    val failed   : TIO.Failure)

  type OnCommit = Callbacks => IO[Unit]

  type CommitFn = OnCommit => TIO.Commit

  def apply[S, A](initial   : A,
                  convInput : S => A,
                  setSelf   : Option[StateFor[A]] => IO[Unit],
                  renderEdit: (A, S => IO[Unit], TIO.Abort, CommitFn) => ReactElement,
                  renderLock: A => ReactElement,
                  renderFail: (A, Failed) => ReactElement): StateFor[A] = {

    val abort = TIO.Abort(setSelf(None))
    val success = TIO.Success(abort)

    def commit(a: A): CommitFn =
      onCommit => {
        def onFailure: TIO.Failure = TIO.Failure.lazily {
          def ff = Failed(() => onCommit(callbacks), () => setSelf(Some(editState(a))))
          setSelf(Some(state(a, ff)))
        }

        def callbacks: Callbacks =
          new Callbacks(
            abort,
            setSelf(Some(state(a, Locked))),
            success,
            onFailure)

        TIO.Commit(onCommit(callbacks))
      }


    def state(a: A, status: Status): StateFor[A] = {
      def render: ReactElement =
        status match {
          case Editing   => renderEdit(a, recvEdit, abort, commit(a))
          case Locked    => renderLock(a)
          case f: Failed => renderFail(a, f)
        }
      StateFor(a, status, () => render)
    }

    def recvEdit: S => IO[Unit] =
      s => setSelf(Some(editState(convInput(s))))

    def editState(a: A): StateFor[A] =
      state(a, Editing)

    editState(initial)
  }

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

  def default[S, A](initial   : A,
                    convInput : S => A,
                    setSelf   : Option[StateFor[A]] => IO[Unit],
                    renderEdit: (A, S => IO[Unit], TIO.Abort, CommitFn) => ReactElement): StateFor[A] =
    apply(initial, convInput, setSelf, renderEdit, defaultRenderLock, defaultRenderFail)

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
