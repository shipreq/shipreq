package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._
import scalaz.Equal
import shipreq.base.util.SetDiff
import shipreq.webapp.client.lib.TCB
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
  case class Failed(retry: () => Callback, resumeEdit: () => Callback) extends Status

  case class StateFor[+A](value: A, status: Status, renderFn: () => ReactElement) {
    @inline def render: ReactElement =
      renderFn()
  }

  @inline implicit def autoOpState[A](s: StateFor[A]): OpStateFor[A] =
    Some(s)

  type State = StateFor[Any]

  type OpState        = Option[State]
  type OpStateFor[+A] = Option[StateFor[A]]

  type SetOpState        = OpState => Callback
  type SetOpStateFor[-A] = OpStateFor[A] => Callback

  class Callbacks(
    val abort    : TCB.Abort,
    val lock     : Callback,
    val succeeded: TCB.Success,
    val failed   : TCB.Failure)

  type OnCommit = Callbacks => Callback
  type CommitFn = OnCommit => TCB.Commit

  type MakeOpStateFor[A] = (A, Status) => OpStateFor[A]

  // ===================================================================================================================

  private def core[S, A](initial   : A,
                         convInput : S => A,
                         setSelf   : SetOpStateFor[A],
//                         abortFn   : MakeOpStateFor[A] => TIO.Abort,
//                         successFn : TIO.Abort => TIO.Success,
                         renderEdit: (A, S => Callback, TCB.Abort, CommitFn) => ReactElement,
                         renderLock: A => ReactElement,
                         renderFail: (A, Failed) => ReactElement): StateFor[A] = {

//    lazy val abort = abortFn(state)
//    lazy val success = successFn(abort)

    val abort = TCB.Abort(setSelf(None))
    val success = TCB.Success(abort)

    def commit(a: A): CommitFn =
      onCommit => {
        def onFailure: TCB.Failure = TCB.Failure.lazily {
          def ff = Failed(() => onCommit(callbacks), () => setSelf(editState(a)))
          setSelf(state(a, ff))
        }

        def callbacks: Callbacks =
          new Callbacks(
            abort,
            setSelf(state(a, Locked)),
            success,
            onFailure)

        TCB.Commit.lazily(onCommit(callbacks))
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

    def recvEdit: S => Callback =
      s => setSelf(editState(convInput(s)))

    def editState(a: A): StateFor[A] =
      state(a, Editing)

    editState(initial)
  }

  def default[S, A](initial   : A,
                    convInput : S => A,
                    setSelf   : SetOpStateFor[A],
                    renderEdit: (A, S => Callback, TCB.Abort, CommitFn) => ReactElement): StateFor[A] =
    core[S, A](
      initial, convInput, setSelf,
//      _ => TIO.Abort(setSelf(None)),
//      TIO.Success(_),
      renderEdit, defaultRenderLock, defaultRenderFail)

  // ===================================================================================================================

  val defaultRenderLock: Any => ReactElement =
    _ => UI.spinner

  val defaultRenderFail: (Any, Failed) => ReactElement =
    (_, f) => renderRetry(f.retry(), f.resumeEdit())

  private def renderRetry(retryFn: => Callback, resumeFn: => Callback) =
    <.div(
      "Network error occurred.",
      <.button("Retry", ^.onClick --> retryFn), // English
      <.button("OK", ^.onClick --> resumeFn)) // English

  // ===================================================================================================================

  @inline implicit def autoUnpackCommitFilter[A](f: CommitFilter[A]): A => OnCommit = f.f

  case class CommitFilter[A](f: A => OnCommit) extends AnyVal {
    def cmapo[B](g: B => Option[A]): CommitFilter[B] =
      CommitFilter(b =>
        g(b) match {
          case Some(a) => f(a)
          case None    => _.abort
        }
      )

    def cmap[B](f: B => A): CommitFilter[B] =
      cmapo(b => Some(f(b)))

    def ignore(f: A => Boolean): CommitFilter[A] =
      cmapo(a => if (f(a)) None else Some(a))

    def ignoreIfEqual(initial: A)(implicit e: Equal[A]): CommitFilter[A] =
      ignore(e.equal(initial, _))

    def ignoreIfEqualO(initial: Option[A])(implicit e: Equal[A]): CommitFilter[A] =
      initial.fold(this)(ignoreIfEqual)

    def cmapToInitial[B: Equal](initial: B)(f: B => A): CommitFilter[B] =
      cmap(f).ignoreIfEqual(initial)

    def setDiff[B](f: SetDiff[B] => A): CommitFilter[SetDiff[B]] =
      cmap(f).ignore(_.isEmpty)
  }
  
}
