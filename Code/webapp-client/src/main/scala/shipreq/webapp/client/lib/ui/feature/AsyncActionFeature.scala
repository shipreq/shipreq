package shipreq.webapp.client.lib.ui.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.macros.GenLens
import monocle.Lens
import monocle.function.At.at
import monocle.std.map.atMap
import shipreq.base.util.UnivEq
import shipreq.webapp.client.lib.TCB
import shipreq.webapp.client.lib.ui.UI

/**
  * Provides the following functionality around async actions:
  *
  * - Status: Locked meaning the async action is in progress.
  * - Status: Failed meaning the async action failed.
  * - Status: None.
  * - When the async action fails, retry functionality.
  * - When the async action fails, cancel functionality.
  * - A UI when locked.
  * - Logic to wrap around async actions to handle state management.
  */
object AsyncActionFeature {

  /**
    * @tparam F The type of async failure.
    */
  sealed trait Status[+F]

  case object Locked extends Status[Nothing]

  case class Failed[F](failure: F, retry: Callback, resumeEdit: Callback) extends Status[F] {
    def retryButton =
      <.button("Retry", ^.onClick --> retry)

    def resumeEditButton =
      <.button("Cancel", ^.onClick --> resumeEdit)
  }

  def renderLocked =
    UI.spinner

  type AsyncCall[+F] = (TCB.Success, F => TCB.Failure) => Callback

  private def genericWrapAsync[F](setStatus: Option[Status[F]] => Callback, call: AsyncCall[F]): Callback = {
    val clearStatus = setStatus(None)

    def onSuccess: TCB.Success =
      TCB Success clearStatus

    def onFailure: F => TCB.Failure =
      f => TCB Failure setStatus(Some(Failed(f, Callback byName doIt, clearStatus)))

    lazy val doIt: Callback =
      call(onSuccess, onFailure) >> setStatus(Some(Locked))

    doIt
  }

  // ===================================================================================================================

  /**
   * Provides the feature for a single value.
   */
  object Single {
    type State[+F] = Option[Status[F]]

    def initState: State[Nothing] =
      None

    final class Feature[S, F]($: CompState.WriteAccess[S], lens: Lens[S, State[F]]) {
      def status(s: S): Option[Status[F]] =
        lens get s

      def wrapAsync(call: AsyncCall[F]): Callback =
        genericWrapAsync[F]($ modState lens.set(_), call)
    }
  }

  // ===================================================================================================================

  /**
   * Provides the feature for a set of values with a key.
   */
  object Keyed {
    type State[K, +F] = Map[K, Status[F]]

    def initState[K: UnivEq]: State[K, Any] =
      UnivEq.emptyMap

    final class Feature[S, K, F]($: CompState.WriteAccess[S], lens: Lens[S, State[K, F]]) {
      private type M = State[K, F]

      private def lensK(c: K): Lens[S, Option[Status[F]]] =
        lens ^|-> at(c)

      def apply(k: K): Single.Feature[S, F] =
        new Single.Feature($, lensK(k))
    }
  }

  // ===================================================================================================================

  /**
   * Provides the feature for a table of rows and columns.
   *
   * Also provides a separate state for entire rows.
   */
  object Table {
    import Keyed.{State => State1D}

    case class RowState[C, +F](rowStatus: Option[Status[F]], cols: State1D[C, F])

    type State[R, C, +F] = Map[R, RowState[C, F]]

    def initState[R: UnivEq, C: UnivEq]: State[R, C, Nothing] =
      UnivEq.emptyMap

    final class Feature[S, R, C, F]($: CompState.WriteAccess[S], lens: Lens[S, State[R, C, F]]) {
      private val rowState_rowStatus = GenLens[RowState[C, F]](_.rowStatus)
      private val rowState_cols      = GenLens[RowState[C, F]](_.cols)
      private val emptyRowState      = RowState[C, F](None, Map.empty)

      private def lensR(r: R): Lens[S, RowState[C, F]] =
        Lens(rowState(r))(n => lens.modify(_.updated(r, n)))

      private def lensC(r: R): Lens[S, State1D[C, F]] =
        lensR(r) ^|-> rowState_cols

      def apply(r: R): Keyed.Feature[S, C, F] =
        new Keyed.Feature($, lensC(r))

      def rowState(r: R)(s: S): RowState[C, F] =
        lens.get(s).getOrElse(r, emptyRowState)

      def rowStatus(r: R)(s: S): Option[Status[F]] =
        rowState(r)(s).rowStatus

      def wrapAsync(r: R, call: AsyncCall[F]): Callback = {
        val l = lensR(r) ^|-> rowState_rowStatus
        genericWrapAsync[F]($ modState l.set(_), call)
      }
    }
  }
}
