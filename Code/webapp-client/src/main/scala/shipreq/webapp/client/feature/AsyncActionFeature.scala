package shipreq.webapp.client.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.MonocleReact._
import monocle.{Lens, Prism}
import scala.annotation.elidable
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
  // TODO Names in this file are terrible: wrapAsync{,D1}; statusD[12]; setD1{,s}

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
  }

  // ===================================================================================================================

  object D1 {
    final class State[A, B, +F](val statusD1: D0.State[F],
                                val values: Map[A, Status[F]],
                                p: Prism[A, B]) extends State.ReadOnly[B, F] {
      @elidable(elidable.FINE)
      override def toString = s"D1.State($statusD1, $values)"

      override def isEmpty: Boolean =
        values.isEmpty && statusD1.isEmpty

      override def apply(key: B): D0.State[F] =
        values.get(p reverseGet key)

      def set[FF >: F](key: B, o: D0.State[FF]): State[A, B, FF] = {
        val m = Dimensions.set1(p)(values, key, o)
        new State(statusD1, m, p)
      }

      def setD1[FF >: F](o: D0.State[FF]): State[A, B, FF] =
        new State(o, values, p)

      override def mapK[C](q: Prism[B, C]): State[A, C, F] =
        new State(statusD1, values, p ^<-? q)

      def mergeInto[FF >: F](parent: State[A, A, FF]): State[A, A, FF] = {
        val m = Dimensions.merge(p)(parent.values, values)
        new State(statusD1, m, Prism.id[A])
      }

//      def iterator: Iterator[(B, Status[F])] =
//          .map(x => p.getOption(x._1) match {
//            case Some(b) => (b, x._2)
//            case None    => null
//          })
//          .filter(_ ne null)

    }

    object State {
      type Simple[K, +F] = State[K, K, F]

      sealed abstract class ReadOnly[K, +F] {
        def isEmpty: Boolean
        def statusD1: D0.State[F]
        def apply(key: K): D0.State[F]
        def mapK[C](q: Prism[K, C]): ReadOnly[C, F]
      }

      implicit def reusabilityState1[K, F]: Reusability[ReadOnly[K, F]] =
        Reusability.byRef || Reusability.whenTrue(_.isEmpty)

      private[AsyncActionFeature] def empty[A, B](p: Prism[A, B]): State[A, B, Nothing] =
        new State(None, Map.empty, p)

      private[AsyncActionFeature] def emptyA[A]: State[A, A, Nothing] =
        empty(Prism.id[A])

      def init[A: UnivEq]: State[A, A, Nothing] =
        emptyA

      def at[A, B, F](b: B): Lens[State[A, B, F], D0.State[F]] =
        Lens((_: State[A, B, F])(b))(o => _.set(b, o))

      def atD1[A, B, F]: Lens[State[A, B, F], D0.State[F]] =
        Lens((_: State[A, B, F]).statusD1)(o => _.setD1(o))
    }

    abstract class Feature[-K, -F] {
      def apply(k: K): D0.Feature[F]
      def wrapAsyncD1(call: AsyncCall[F]): Callback
    }

    def Feature[A, B, F]($: CompState.WriteAccess[State[A, B, F]]): Feature[B, F] =
      new Feature[B, F] {
        override def apply(b: B) =
          D0.Feature($ zoomL State.at(b))

        override def wrapAsyncD1(call: AsyncCall[F]) =
          genericWrapAsync[F]($.zoomL(State.atD1) setState _, call)
      }
  }

  // ===================================================================================================================

  object D2 {

    final class State[A2, B2, A1, B1, F](val statusD2: D0.State[F],
                                         val values: Map[A2, D1.State[A1, A1, F]],
                                         f2: B2 => A2,
                                         p1: Prism[A1, B1]) extends State.ReadOnly[B2, B1, F] {
      @elidable(elidable.FINE)
      override def toString = s"D2.State($statusD2, $values)"

      override def isEmpty: Boolean =
        values.isEmpty && statusD2.isEmpty

      override def apply(key: B2): D1.State[A1, B1, F] =
        values.get(f2(key)) match {
          case Some(s) => s mapK p1
          case None    => D1.State.empty(p1)
        }

      def set(k: B2, v: D1.State[A1, B1, F]): State[A2, B2, A1, B1, F] = {
        val m = Dimensions.set2(values)(f2(k), v mergeInto _.getOrElse(D1.State.emptyA))(_.isEmpty)
        new State(statusD2, m, f2, p1)
      }

      def mod(k: B2, f: D1.State[A1, B1, F] => D1.State[A1, B1, F]): State[A2, B2, A1, B1, F] =
        set(k, f(apply(k)))

      override def mapK2[C2](f: C2 => B2): State[A2, C2, A1, B1, F] =
        new State(statusD2, values, f2 compose f, p1)

      override def mapK1[C1](q: Prism[B1, C1]): State[A2, B2, A1, C1, F] =
        new State(statusD2, values, f2, p1 ^<-? q)
    }

    object State {
      type Simple[K2, K1, F] = State[K2, K2, K1, K1, F]

      def init[K2: UnivEq, K1: UnivEq, F]: State[K2, K2, K1, K1, F] =
        new State(None, UnivEq.emptyMap, identity[K2], Prism.id[K1])

      sealed abstract class ReadOnly[K2, K1, +F] {
        def isEmpty: Boolean
        def apply(key: K2): D1.State.ReadOnly[K1, F]
        def statusD2: D0.State[F]
        def mapK2[K](f: K => K2): ReadOnly[K, K1, F]
        def mapK1[K](q: Prism[K1, K]): ReadOnly[K2, K, F]
      }

      implicit def reusabilityState2[K2, K1, F]: Reusability[ReadOnly[K2, K1, F]] =
        Reusability.byRef || Reusability.whenTrue(_.isEmpty)

      def at[A2, B2, A1, B1, F](k: B2): Lens[State[A2, B2, A1, B1, F], D1.State[A1, B1, F]] =
        Lens((_: State[A2, B2, A1, B1, F])(k))(o => _.set(k, o))
    }

    abstract class Feature[-K2, -K1, -F] {
      def apply(k2: K2): D1.Feature[K1, F]
      def setD1s(ks: Iterable[K2], value: => D0.State[F]): Callback

      final def setD1(k: K2, value: => D0.State[F]): Callback =
        setD1s(k :: Nil, value)
    }

    def Feature[A2, B2, A1, B1, F]($: CompState.WriteAccess[State[A2, B2, A1, B1, F]]): Feature[B2, B1, F] =
      new Feature[B2, B1, F] {
        override def apply(b: B2) =
          D1.Feature($ zoomL State.at(b))

        override def setD1s(ks: Iterable[B2], value: => D0.State[F]): Callback =
          Callback.ifTrue(ks.nonEmpty,
            $.modState { s =>
              val v = value
              ks.foldLeft(s)(_.mod(_, _ setD1 v))
            }
          )
      }
  }
}
