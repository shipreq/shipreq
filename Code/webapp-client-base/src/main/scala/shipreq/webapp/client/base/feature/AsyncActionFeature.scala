package shipreq.webapp.client.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.MonocleReact._
import monocle.Lens
import scala.annotation.elidable
import shipreq.base.util.Intersection
import shipreq.base.util.univeq._
import shipreq.webapp.client.base.data.TCB

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
  // TODO Names in this file are terrible: wrapAsync{,D1}; statusD[12]; setD1{,s}

  /**
    * @tparam F The type of async failure.
    */
  sealed abstract class Status[+F]

  case object Locked extends Status[Nothing]

  final case class Failed[F](failure: F, retry: Callback, resumeEdit: Callback) extends Status[F] {
    def retryButton =
      <.button("Retry", ^.onClick --> retry)

    def resumeEditButton =
      <.button("Abort", ^.onClick --> resumeEdit)
  }

  @inline implicit class AAFStringStatusOps(private val s: Status[String]) extends AnyVal {
    def render: ReactElement =
      s match {
        case Locked            => renderLocked
        case f: Failed[String] => <.div(f.failure, f.retryButton, f.resumeEditButton)
      }
  }

  @inline implicit class AAFState0Ops(private val s: D0.State[String]) extends AnyVal {
    def renderOr[A](a: => A)(implicit ev: ReactElement => A): A =
      s.fold(a)(x => ev(x.render))
  }

  implicit def reusabilityStatus[F]: Reusability[Status[F]] =
    Reusability.byRef

  def renderLocked =
    <.div("LOCKED") // TODO Temp

  type AsyncCall[+F] = (TCB.Success, F => TCB.Failure) => Callback

  private def genericWrapAsync[F](setStatus: Option[Status[F]] => Callback, call: AsyncCall[F]): Callback = {
    val clearStatus = setStatus(None)

    def onSuccess: TCB.Success =
      TCB Success clearStatus

    def onFailure: F => TCB.Failure =
      f => TCB Failure setStatus(Some(Failed(f, Callback byName doIt, clearStatus)))

    lazy val doIt: Callback =
      // Switching this around breaks tests' MockServer's order of events.
      // i.e. it will call onSuccess which clears the status, and then set it to locked.
      setStatus(Some(Locked)) >> call(onSuccess, onFailure)

    doIt
  }

  // ===================================================================================================================

  /**
   * Provides the feature for a single value.
   */
  object D0 {
    type State[+F] = Option[Status[F]]

    def initState: State[Nothing] =
      None

    abstract class Feature[-F] {
      def wrapAsync(call: AsyncCall[F]): Callback
    }

    def Feature[F]($: CompState.WriteAccess[State[F]]): Feature[F] =
      new Feature[F] {
        override def wrapAsync(call: AsyncCall[F]) =
          genericWrapAsync[F]($ setState _, call)
      }

    def nopFeature: Feature[Any] =
      new Feature[Any] {
        override def wrapAsync(call: AsyncCall[Any]) =
          Callback.empty
      }
  }

  // ===================================================================================================================

  object D1 {
    final class State[A, B, +F](val statusD1: D0.State[F],
                                val values: Map[A, Status[F]],
                                i: Intersection[A, B]) extends State.ReadOnly[B, F] {
      @elidable(elidable.FINE)
      override def toString = s"D1.State($statusD1, $values)"

      override def isEmpty: Boolean =
        values.isEmpty && statusD1.isEmpty

      override def apply(key: B): D0.State[F] =
        i.reverse.fold(key, values.get)(D0.initState)

      def set[FF >: F](key: B, o: D0.State[FF]): State[A, B, FF] = {
        val m = Dimensions.set1(i)(values, key, o)
        new State(statusD1, m, i)
      }

      def setD1[FF >: F](o: D0.State[FF]): State[A, B, FF] =
        new State(o, values, i)

      override def mapK[C](j: Intersection[B, C]): State[A, C, F] =
        new State(statusD1, values, i composeIntersection j)

      def mergeInto[FF >: F](parent: State[A, A, FF]): State[A, A, FF] = {
        val m = Dimensions.merge(i.getOption)(parent.values, values)
        new State(statusD1, m, Intersection.id[A])
      }
    }

    object State {
      type Simple[K, +F] = State[K, K, F]

      sealed abstract class ReadOnly[K, +F] {
        def isEmpty: Boolean
        def statusD1: D0.State[F]
        def apply(key: K): D0.State[F]
        def mapK[C](q: Intersection[K, C]): ReadOnly[C, F]
      }

      implicit def reusabilityState1[K, F]: Reusability[ReadOnly[K, F]] =
        Reusability.byRef || Reusability.whenTrue(_.isEmpty)

      private[AsyncActionFeature] def empty[A, B](p: Intersection[A, B]): State[A, B, Nothing] =
        new State(None, Map.empty, p)

      private[AsyncActionFeature] def emptyA[A]: State[A, A, Nothing] =
        empty(Intersection.id[A])

      def init[A: UnivEq]: State[A, A, Nothing] =
        emptyA

      def at[A, B, F](b: B): Lens[State[A, B, F], D0.State[F]] =
        Lens((_: State[A, B, F])(b))(o => _.set(b, o))

      def atD1[A, B, F]: Lens[State[A, B, F], D0.State[F]] =
        Lens((_: State[A, B, F]).statusD1)(o => _.setD1(o))
    }

    abstract class Feature[K, -F] {
      def apply(k: K): D0.Feature[F]
      def mapK[C](j: Intersection[K, C]): Feature[C, F]
      def wrapAsyncD1(call: AsyncCall[F]): Callback
    }

    object Feature {
      private final class Impl[S, A, B, -F]($: CompState.WriteAccess[State[S, A, F]],
                                            i: Intersection[A, B]) extends Feature[B, F] {
        override def apply(b: B) =
          i.reverse.fold(b, a => D0.Feature($ zoomL State.at(a)))(D0.nopFeature)

        override def mapK[C](j: Intersection[B, C]) =
          new Impl($, i composeIntersection j)

        override def wrapAsyncD1(call: AsyncCall[F]) =
          genericWrapAsync[F]($.zoomL(State.atD1) setState _, call)
      }

      @inline def apply[S, A, F]($: CompState.WriteAccess[State[S, A, F]]): Feature[A, F] =
        apply($, Intersection.id[A])

      def apply[S, A, B, F]($: CompState.WriteAccess[State[S, A, F]], i: Intersection[A, B]): Feature[B, F] =
        new Impl($, i)

      def nop[K]: Feature[K, Any] =
        new Feature[K, Any] {
          override def apply(k: K)                       = D0.nopFeature
          override def mapK[C](j: Intersection[K, C])    = nop
          override def wrapAsyncD1(call: AsyncCall[Any]) = Callback.empty
        }
      }
  }

  // ===================================================================================================================

  object D2 {

    final class State[A2, B2, A1, B1, F](val statusD2: D0.State[F],
                                         val values: Map[A2, D1.State[A1, A1, F]],
                                         i2: Intersection[A2, B2],
                                         i1: Intersection[A1, B1]) extends State.ReadOnly[B2, B1, F] {
      @elidable(elidable.FINE)
      override def toString = s"D2.State($statusD2, $values)"

      override def isEmpty: Boolean =
        values.isEmpty && statusD2.isEmpty

      override def apply(key: B2): D1.State[A1, B1, F] =
        i2.reverse.fold(key, values.get)(None) match {
          case Some(s) => s mapK i1
          case None    => D1.State.empty(i1)
        }

      def set(k: B2, v: D1.State[A1, B1, F]): State[A2, B2, A1, B1, F] = {
        val m = Dimensions.set2(i2)(values)(k, v mergeInto _.getOrElse(D1.State.emptyA), _.isEmpty)
        new State(statusD2, m, i2, i1)
      }

      def mod(k: B2, f: D1.State[A1, B1, F] => D1.State[A1, B1, F]): State[A2, B2, A1, B1, F] =
        set(k, f(apply(k)))

      override def mapK2[C2](j: Intersection[B2, C2]): State[A2, C2, A1, B1, F] =
        new State(statusD2, values, i2 composeIntersection j, i1)

      override def mapK1[C1](j: Intersection[B1, C1]): State[A2, B2, A1, C1, F] =
        new State(statusD2, values, i2, i1 composeIntersection j)

      override def iterator: Iterator[(B2, D1.State[A1, B1, F])] =
        Dimensions.iterator(i2.getOption, values)(_ mapK i1)
    }

    object State {
      type Simple[K2, K1, F] = State[K2, K2, K1, K1, F]

      def init[K2: UnivEq, K1: UnivEq, F]: State[K2, K2, K1, K1, F] =
        new State(None, UnivEq.emptyMap, Intersection.id[K2], Intersection.id[K1])

      sealed abstract class ReadOnly[K2, K1, +F] {
        def isEmpty: Boolean
        def apply(key: K2): D1.State.ReadOnly[K1, F]
        def statusD2: D0.State[F]
        def mapK2[K](f: Intersection[K2, K]): ReadOnly[K, K1, F]
        def mapK1[K](q: Intersection[K1, K]): ReadOnly[K2, K, F]
        def iterator: Iterator[(K2, D1.State.ReadOnly[K1, F])]
      }

      implicit def reusabilityState2[K2, K1, F]: Reusability[ReadOnly[K2, K1, F]] =
        Reusability.byRef || Reusability.whenTrue(_.isEmpty)

      def at[A2, B2, A1, B1, F](k: B2): Lens[State[A2, B2, A1, B1, F], D1.State[A1, B1, F]] =
        Lens((_: State[A2, B2, A1, B1, F])(k))(o => _.set(k, o))
    }

    abstract class Feature[K2, K1, -F] {
      def apply(k2: K2): D1.Feature[K1, F]
      def mapK1[C](j: Intersection[K1, C]): Feature[K2, C, F]
      def mapK2[C](j: Intersection[K2, C]): Feature[C, K1, F]
      def setD1s(ks: Iterable[K2], value: => D0.State[F]): Callback

      final def setD1(k: K2, value: => D0.State[F]): Callback =
        setD1s(k :: Nil, value)
    }

    object Feature {
      private final class Impl[S2, A2, B2, S1, A1, B1, -F]($: CompState.WriteAccess[State[S2, A2, S1, A1, F]],
                                                           i2: Intersection[A2, B2],
                                                           i1: Intersection[A1, B1]) extends Feature[B2, B1, F] {
        override def apply(b: B2) =
          i2.reverse.fold(b, a => D1.Feature($ zoomL State.at(a), i1))(D1.Feature.nop)

        override def mapK1[C](j: Intersection[B1, C]) =
          new Impl($, i2, i1 composeIntersection j)

        override def mapK2[C](j: Intersection[B2, C]) =
          new Impl($, i2 composeIntersection j, i1)

        override def setD1s(ks: Iterable[B2], value: => D0.State[F]): Callback =
          Callback.unless(ks.isEmpty)(
            $.modState { s =>
              val v = value
              ks.foldLeft(s)((q, b) => i2.reverse.fold(b, q.mod(_, _ setD1 v)) {
                Dimensions.warnDiscard(b)
                q
              })
            }
          )
      }

      def apply[S2, A2, S1, A1, F]($: CompState.WriteAccess[State[S2, A2, S1, A1, F]]): Feature[A2, A1, F] =
        new Impl($, Intersection.id[A2], Intersection.id[A1])

      implicit def reusabilityFeature[A, B, C]: Reusability[Feature[A, B, C]] =
        Reusability.byRef
    }
  }
}
