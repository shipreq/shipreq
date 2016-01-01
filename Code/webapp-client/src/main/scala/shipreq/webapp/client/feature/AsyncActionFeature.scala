package shipreq.webapp.client.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.macros.GenLens
import monocle.Lens
import monocle.function.At.at
import monocle.std.map.atMap
import shipreq.base.util.UnivEq
import shipreq.webapp.client.app.Assets
import shipreq.webapp.client.data.TCB

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
      <.button("Abort", ^.onClick --> resumeEdit)
  }

  implicit def reusabilityStatus[F]: Reusability[Status[F]] =
    Reusability.byRef

  def renderLocked =
    Assets.spinner

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

    type State[R, C, +F] = Map[R, RowState[C, F]]

    case class RowState[C, +F](rowStatus: Option[Status[F]], cols: State1D[C, F])

    def initState[R: UnivEq, C: UnivEq]: State[R, C, Nothing] =
      UnivEq.emptyMap

    private[this] val _emptyRowState = RowState[Any, Nothing](None, Map.empty)

    def emptyRowState[C]: RowState[C, Nothing] =
      _emptyRowState.asInstanceOf[RowState[C, Nothing]]

    def get[R, C, F](s: State[R, C, F])(r: R): RowState[C, F] =
      s.getOrElse(r, emptyRowState)

    final class Feature[S, R, C, F]($: CompState.WriteAccess[S], lens: Lens[S, State[R, C, F]]) extends FeatureAnon[R, C, F] {
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

      def hasState_?(r: R, c: C)(s: S): Boolean = {
        val rs = rowState(r)(s)
        rs.rowStatus.isDefined || rs.cols.get(c).isDefined
      }

      override def wrapAsync(r: R, call: AsyncCall[F]): Callback = {
        val l = lensR(r) ^|-> rowState_rowStatus
        genericWrapAsync[F]($ modState l.set(_), call)
      }

      override def setRowStatuses(rs: Iterable[R], value: => Option[Status[F]]): Callback =
        Callback.byName {
          val v = value
          val f = rowState_rowStatus set v
          val modAll = rs.iterator.map(lensR(_) modify f).foldLeft[S => S](identity)(_ compose _)
          $ modState modAll
        }
    }

    trait FeatureAnon[R, C, F] {
      def wrapAsync(r: R, call: AsyncCall[F]): Callback
      def setRowStatuses(rs: Iterable[R], value: => Option[Status[F]]): Callback

      final def setRowStatus(r: R, value: => Option[Status[F]]): Callback =
        setRowStatuses(r :: Nil, value)
    }

    implicit def reusabilityFeatureAnon[R, C, F]: Reusability[FeatureAnon[R, C, F]] = Reusability.byRef
    implicit def reusabilityState      [R, C, F]: Reusability[State      [R, C, F]] = Reusability.byRef
    implicit def reusabilityRowState      [C, F]: Reusability[RowState      [C, F]] = Reusability.byRef

    def Fix[R: UnivEq, C: UnivEq, F] = new Fix[R, C, F]
    final class Fix[R: UnivEq, C: UnivEq, F] {
      type Row         = R
      type Col         = C
      type Failure     = F
      type TableState  = AsyncActionFeature.Table.State[R, C, F]
      type RowState    = AsyncActionFeature.Table.RowState[C, F]
      type ColStates   = State1D[C, F]
      type Single      = AsyncActionFeature.Single.State[F]
      type Status      = AsyncActionFeature.Status[F]
      type Failed      = AsyncActionFeature.Failed[F]
      type Feature[S]  = AsyncActionFeature.Table.Feature[S, R, C, F]
      type FeatureAnon = AsyncActionFeature.Table.FeatureAnon[R, C, F]

      def initState = Table.initState[R, C]

      def Feature[S]($: CompState.WriteAccess[S])(lens: Lens[S, State[R, C, F]]): Feature[S] =
        new AsyncActionFeature.Table.Feature($, lens)

      def get(s: State[R, C, F])(r: R): RowState =
        Table.get(s)(r)
    }
  }
}
