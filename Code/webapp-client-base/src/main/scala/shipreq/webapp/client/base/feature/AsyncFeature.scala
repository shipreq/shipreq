package shipreq.webapp.client.base.feature

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.MonocleReact._
import scala.reflect.ClassTag
import shipreq.base.util.{Intersection, Optics}
import shipreq.webapp.client.base.data.TCB

/** Provides the following functionality around async actions:
  *
  * - A status to use between initiation and successful completion of an async action. Statuses are:
  *   - Locked: the async action is in progress, awaiting a response (or timeout).
  *   - Failed: the async action failed.
  * - When an async action has failed, this provides:
  *   - Retry functionality.
  *   - Cancel/abort functionality.
  *
  * Usage: Top-Most Component
  * =========================
  *
  * import `Implicits._`
  * Add `State.Dn` to the top-most component's state.
  * Initialise it with `State.initDn`.
  * In the component backend, add `val asyncFeature = Feature.Dn.init(…)`.
  * In the render method, pass `asyncFeature` and/or `asyncFeature.toProps(state)` to children.
  *
  * Usage: Components
  * =================
  *
  * To just inspect the current async state, add `ReadOnly.Dn` to the components props.
  * For write-access to async state, add `Feature.Dn` to the components props.
  * For both of the above, add `Props.Dn` to the components props instead.
  *
  * Intended usage is that
  * - async tasks are run through `Feature.D0` so that state is managed
  * - state is inspected when rendering to show the async status to the user
  * - state is inspected to prevent multiple async calls being in flight
  */
object AsyncFeature {

  /**
    * @tparam F An explanation of why some async action failed.
    */
  sealed abstract class Status[+F]

  object Status {

    /** An async action is in progress, result pending */
    case object Locked extends Status[Nothing]

    /** @param failure An explanation of why some async action failed.
      * @param cancel Return to the state prior to initiating the async action.
      *               For editors, this restores the editor, makes it editable again and retains its dirty state.
      * @tparam F An explanation of why some async action failed.
      */
    final case class Failed[+F](failure: F, retry: Callback, cancel: Callback) extends Status[F]

    implicit def reusability[F]: Reusability[Status[F]] =
      Reusability.byRef
  }

  /** Provides two callbacks for you to use in your async logic:
    * one for you to call on async success
    * one for you to call on async failure
    */
  type AsyncCall[+F] = (TCB.Success, F => TCB.Failure) => Callback

  private val _doNothingReusability = Reusable.byRef(new AnyRef)
  private def withDoNothingReusability[A](a: => A): Reusable[A] =
    _doNothingReusability.map(_ => a)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Implicits {
    implicit class AF_StateD1[K, F](private val self: State.D1[K, F]) extends AnyVal {
      def toReadOnly: ReadOnly.D1[K, F] = ReadOnly.D1(self)
    }
    implicit class AF_StateD2[K2, K1, F](private val self: State.D2[K2, K1, F]) extends AnyVal {
      def toReadOnly: ReadOnly.D2[K2, K1, F] = ReadOnly.D2(self)
    }
    implicit class AF_FeatureD1[K, F](private val self: Feature.D1[K, F]) extends AnyVal {
      def toProps(r: ReadOnly.D1[K, F]): Props.D1[K, F] = Props.D1(self, r)
    }
    implicit class AF_FeatureD2[K2, K1, F](private val self: Feature.D2[K2, K1, F]) extends AnyVal {
      def toProps(r: ReadOnly.D2[K2, K1, F]): Props.D2[K2, K1, F] = Props.D2(self, r)
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  /** This is only to be used when creating the top-most State object */
  object State {
    type D0[+F] = Option[Status[F]]
    type D1[K, +F] = Map[K, Status[F]]
    type D2[K2, K1, +F] = Map[K2, D1[K1, F]]

    def initD0: D0[Nothing] = None
    def initD1[K]: D1[K, Nothing] = Map.empty
    def initD2[K2, K1]: D2[K2, K1, Nothing] = Map.empty
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  /** State in a form meant to be passed to components */
  object ReadOnly {

    type D0[+F] = State.D0[F]

    // =================================================================================================================

    trait D1[K, +F] {
      def isEmpty: Boolean
      def apply(key: K): D0[F]
      def mapKey[J](j: Intersection[K, J]): D1[J, F]
    }

    object D1 {
      def apply[K, F](state: State.D1[K, F]): D1[K, F] =
        mapped(state, Intersection.id)

      def mapped[SK, K, F](state: State.D1[SK, F], i: Intersection[SK, K]): D1[K, F] =
        new D1[K, F] {
          override def isEmpty: Boolean =
            state.isEmpty

          override def apply(key: K): D0[F] =
            i.reverse.getOption(key).flatMap(state.get)

          override def mapKey[J](j: Intersection[K, J]): D1[J, F] =
            mapped(state, i <=> j)
      }

      implicit def reusability[K, F]: Reusability[D1[K, F]] =
        Reusability.byRef || Reusability.when(_.isEmpty)
    }

    // =================================================================================================================

    trait D2[K2, K1, +F] {
      def isEmpty: Boolean
      def apply(key: K2): D1[K1, F]
      def mapKey2[J](j: Intersection[K2, J]): D2[J, K1, F]
      def mapKey1[J](j: Intersection[K1, J]): D2[K2, J, F]
      def iterator: Iterator[(K2, D1[K1, F])]
    }

    object D2 {
      def apply[K2, K1, F](state: State.D2[K2, K1, F]): D2[K2, K1, F] =
        mapped(state, Intersection.id, Intersection.id)

      def mapped[SK2, SK1, K2, K1, F](state: State.D2[SK2, SK1, F],
                                      i2: Intersection[SK2, K2],
                                      i1: Intersection[SK1, K1]): D2[K2, K1, F] =
        new D2[K2, K1, F] {
          def isEmpty: Boolean =
            state.isEmpty

          def apply(key: K2): D1[K1, F] =
            D1.mapped(i2.reverse.fold(key, state.get)(None).orEmptyMap, i1)

          def mapKey2[J](j: Intersection[K2, J]): D2[J, K1, F] =
            mapped(state, i2 <=> j, i1)

          def mapKey1[J](j: Intersection[K1, J]): D2[K2, J, F] =
            mapped(state, i2, i1 <=> j)

          def iterator: Iterator[(K2, D1[K1, F])] =
            Dimensions.iterator(i2.getOption, state)(D1.mapped(_, i1))
      }

      implicit def reusability[K2, K1, F]: Reusability[D2[K2, K1, F]] =
        Reusability.byRef || Reusability.when(_.isEmpty)
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Feature {

    type D0[-F] = AsyncCall[F] ~=> Callback

    object D0 {
      def init[F]($: StateAccessPure[State.D0[F]]): D0[F] =
        apply(Reusable.fn.state($).set)

      def apply[F](setStateFn: State.D0[F] ~=> Callback): D0[F] =
        setStateFn.map(setState => call => {
          val clearStatus = setState(None)

          def onSuccess: TCB.Success =
            TCB Success clearStatus

          def onFailure: F => TCB.Failure =
            f => TCB Failure setState(Some(Status.Failed(f, Callback byName doIt, clearStatus)))

          lazy val doIt: Callback =
            // Switching this around breaks tests' MockServer's order of events.
            // i.e. it will call onSuccess which clears the status, and then set it to locked.
            setState(Some(Status.Locked)) >> call(onSuccess, onFailure)

          doIt
        })

      val doNothing: D0[Any] =
        Reusable.byRef(_ => Callback.empty)
    }

    // =================================================================================================================

    type D1[K, -F] = Reusable[D1.Interface[K, F]]

    object D1 {
      trait Interface[K, -F] {
        def apply(k: K): D0[F]
        def mapKey[J](j: Intersection[K, J]): D1[J, F]
      }

      def apply[SK: Reusability: ClassTag, K, F]($: Reusable[StateAccessPure[State.D1[SK, F]]],
                                                 i: Intersection[SK, K]): D1[K, F] =
        // Doesn't factor Intersection into reusability because they're coherent
        $.map(_ => new Interface[K, F] {
          override def apply(k: K): D0[F] =
            i.reverse.fold[D0[F]](k,
              sk => D0(Reusable.ap($, Reusable.implicitly(sk))(($, sk) =>
                $.zoomStateL(Optics.mapValue(sk)).setState(_)))
            )(D0.doNothing)

          override def mapKey[J](j: Intersection[K, J]) =
            D1($, i <=> j)
        })

      def doNothing[K]: D1[K, Any] =
        withDoNothingReusability(new Interface[K, Any] {
          override def apply(k: K)                      = D0.doNothing
          override def mapKey[J](j: Intersection[K, J]) = doNothing
        })
    }

    // =================================================================================================================

    type D2[K2, K1, -F] = Reusable[D2.Interface[K2, K1, F]]

    object D2 {
      trait Interface[K2, K1, -F] {
        def apply(k2: K2): D1[K1, F]
        def mapKey1[C](j: Intersection[K1, C]): D2[K2, C, F]
        def mapKey2[C](j: Intersection[K2, C]): D2[C, K1, F]
        def setBulk(k2s: Iterable[K2], k1: K1, value: => State.D0[F]): Callback
      }

      def apply[SK2: Reusability : ClassTag, SK1: Reusability : ClassTag, K2, K1, F]($: Reusable[StateAccessPure[State.D2[SK2, SK1, F]]],
                                                                                     i2: Intersection[SK2, K2],
                                                                                     i1: Intersection[SK1, K1]): D2[K2, K1, F] =
        // Doesn't factor Intersections into reusability because they're coherent
        $.map(_ => new Interface[K2, K1, F] {
          def lensAt(sk2: SK2) = Optics.innerMap[SK2, SK1, Status[F]](sk2)

          override def apply(k: K2): D1[K1, F] =
            i2.reverse.fold[D1[K1, F]](k,
              sk => D1(Reusable.ap($, Reusable.implicitly(sk))(($, sk) =>
                $.zoomStateL(lensAt(sk))), i1)
            )(D1.doNothing)

          override def mapKey2[J](i: Intersection[K2, J]): D2[J, K1, F] =
            D2($, i2 <=> i, i1)

          override def mapKey1[J](i: Intersection[K1, J]): D2[K2, J, F] =
            D2($, i2, i1 <=> i)

          override def setBulk(k2s: Iterable[K2], k1: K1, value: => State.D0[F]): Callback =
            Callback.unless(k2s.isEmpty)(
              i1.reverse.getOption(k1) match {
                case Some(sk1) =>
                  $.modState { initialState =>
                    val setValue = value.toMapEntrySetFn[SK1]
                    k2s.foldLeft(initialState)((s, k2) => i2.reverse.getOption(k2) match {
                      case Some(sk2) => lensAt(sk2).modify(setValue(_, sk1))(s)
                      case None      => Dimensions.warnDiscard(k2); s
                    })
                  }
                case None =>
                  Dimensions.warnDiscard(k1); Callback.empty
              }
            )

        })

      def init[K2: Reusability : ClassTag, K1: Reusability : ClassTag, F]($: StateAccessPure[State.D2[K2, K1, F]]): D2[K2, K1, F] =
        apply(Reusable.byRef($), Intersection.id, Intersection.id)
    }

  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Props {

    final case class D0[F](feature: Feature.D0[F], state: ReadOnly.D0[F])

    final case class D1[K, F](feature: Feature.D1[K, F], state: ReadOnly.D1[K, F]) {
      def apply(k: K): D0[F] =
        D0(feature(k), state(k))

      def mapKey[J](i: Intersection[K, J]): D1[J, F] =
        D1(feature.mapKey(i), state.mapKey(i))
    }

    final case class D2[K2, K1, F](feature: Feature.D2[K2, K1, F], state: ReadOnly.D2[K2, K1, F]) {
      def apply(k: K2): D1[K1, F] =
        D1(feature(k), state(k))

      def mapKey2[J](i: Intersection[K2, J]): D2[J, K1, F] =
        D2(feature.mapKey2(i), state.mapKey2(i))

      def mapKey1[J](i: Intersection[K1, J]): D2[K2, J, F] =
        D2(feature.mapKey1(i), state.mapKey1(i))
    }
  }
}
