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

  type SetOpState        = (OpState, Callback) => Callback
  type SetOpStateFor[-A] = (OpStateFor[A], Callback) => Callback

  class Callbacks(
    val abort    : TCB.Abort,
    val lock     : Callback,
    val succeeded: TCB.Success,
    val failed   : TCB.Failure)

  type OnCommit = Callbacks => Callback
  type CommitFn = OnCommit => TCB.Commit

  type MakeOpStateFor[A] = (A, Status) => OpStateFor[A]

  type PostAbort = AbortType => TCB.Abort
  type PostLock  = Callback

  type RenderEdit[-A, +S] = (A, S => Callback, TCB.Abort, CommitFn) => ReactElement

  sealed trait AbortType
  case object AbortFromEditor extends AbortType
  case object AbortAfterSuccess extends AbortType

  // ===================================================================================================================

  private def core[S, A](initial   : A,
                         convInput : S => A,
                         setSelfFn : SetOpStateFor[A],
                         postAbort : PostAbort,
                         postLock  : PostLock,
//                         abortFn   : MakeOpStateFor[A] => TIO.Abort,
//                         successFn : TIO.Abort => TIO.Success,
                         renderEdit: RenderEdit[A, S],
                         renderLock: A => ReactElement,
                         renderFail: (A, Failed) => ReactElement): StateFor[A] = {

    @inline def setSelf(a: OpStateFor[A], cb: Callback = Callback.empty) =
      setSelfFn(a, cb)

//    lazy val abort = abortFn(state)
//    lazy val success = successFn(abort)

    def abort(t: AbortType): TCB.Abort =
      TCB.Abort(setSelf(None, postAbort(t).cb))

    val abortEdit = abort(AbortFromEditor)
    val success = TCB.Success(abort(AbortAfterSuccess))

    def commit(a: A): CommitFn =
      onCommit => {
        def onFailure: TCB.Failure = TCB.Failure.lazily {
          def ff = Failed(() => onCommit(callbacks), () => setSelf(editState(a)))
          setSelf(state(a, ff))
        }

        def callbacks: Callbacks =
          new Callbacks(
            abortEdit,
            setSelf(state(a, Locked), postLock),
            success,
            onFailure)

        TCB.Commit.lazily(onCommit(callbacks))
      }


    def state(a: A, status: Status): StateFor[A] = {
      def render: ReactElement =
        status match {
          case Editing   => renderEdit(a, recvEdit, abortEdit, commit(a))
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
                    postAbort : PostAbort,
                    postLock  : PostLock,
                    renderEdit: RenderEdit[A, S]): StateFor[A] =
    core[S, A](
      initial, convInput, setSelf,
      postAbort, postLock,
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
