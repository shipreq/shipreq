package shipreq.webapp.base.feature

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.MonocleReact._
import japgolly.univeq.UnivEq
import scala.reflect.ClassTag
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{Intersection, Optics}
import shipreq.webapp.base.lib.BaseReusability._

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
  * Add `State.Dₙ` to the top-most component's state.
  * Initialise it with `State.initDₙ`.
  * In the component backend, add `val asyncFeature = Write.Dₙ.init(…)`.
  * In the render method, pass one of the following to children:
  *   - `state.toRead` for read-access
  *   - `asyncFeature` for write-access
  *   - `asyncFeature.toProps(state)` for read/write-access
  *
  * Usage: Components
  * =================
  *
  * To just inspect the current async state, add `Read.Dₙ` to the components props.
  * For write-access to async state, add `Write.Dₙ` to the components props.
  * For both of the above, add `ReadWrite.Dn` to the components props instead.
  *
  * Intended usage is that
  * - async tasks are run through `Write.D0` so that state is managed
  * - state is inspected when rendering to show the async status to the user
  * - state is inspected to prevent multiple async calls being in flight
  */
object AsyncFeature {

  def isInProgress(s: State.D0[Any]): Boolean =
    s.exists {
      case Status.InProgress => true
      case _                 => false
    }

  /**
    * @tparam F An explanation of why some async action failed.
    */
  sealed abstract class Status[+F]

  object Status {

    /** An async action is in progress, result pending */
    case object InProgress extends Status[Nothing]

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
    *
    * The reason I'm keeping this as is instead of upgrading it all to work with AsyncCallbacks is that on failure we
    * store a retry Callback. The only way to ensure that retry does exactly the same thing (as opposed to just the
    * first steps and then this feature not knowing about external map & flatmaps) is that the complete and entire
    * callback must be passed to this feature.
    */
  type AsyncCall[+F] = (Callback, F => Callback) => Callback

  private val _doNothingReusability = Reusable.byRef(new AnyRef)
  private def withDoNothingReusability[A](a: => A): Reusable[A] =
    _doNothingReusability.map(_ => a)

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Implicits {
    implicit class AF_StateD1[K, F](private val self: State.D1[K, F]) extends AnyVal {
      def toRead: Read.D1[K, F] = Read.D1(self)
    }
    implicit class AF_StateD2[K2, K1, F](private val self: State.D2[K2, K1, F]) extends AnyVal {
      def toRead: Read.D2[K2, K1, F] = Read.D2(self)
    }
    implicit class AF_FeatureD1[K, F](private val self: Write.D1[K, F]) extends AnyVal {
      def toReadWrite(r: Read.D1[K, F]): ReadWrite.D1[K, F] = ReadWrite.D1(self, r)
    }
    implicit class AF_FeatureD2[K2, K1, F](private val self: Write.D2[K2, K1, F]) extends AnyVal {
      def toReadWrite(r: Read.D2[K2, K1, F]): ReadWrite.D2[K2, K1, F] = ReadWrite.D2(self, r)
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  /** This is only to be used when creating the top-most State object */
  object State {
    type D0[+F]         = Option[Status[F]]
    type D1[K, +F]      = Map[K, Status[F]]
    type D2[K2, K1, +F] = Map[K2, D1[K1, F]]

    def initD0                        : D0[Nothing]         = None
    def initD1[K: UnivEq]             : D1[K, Nothing]      = UnivEq.emptyMap
    def initD2[K2: UnivEq, K1: UnivEq]: D2[K2, K1, Nothing] = UnivEq.emptyMap
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  /** State in a form meant to be passed to components */
  object Read {

    type D0[+F] = State.D0[F]

    // =================================================================================================================

    trait D1[K, +F] {
      def isEmpty: Boolean
      def apply(key: K): D0[F]
      def mapKey[J](j: Intersection[K, J]): D1[J, F]
      def iterator: Iterator[(K, Status[F])]
      def keySet: Set[K]

      final def either[K2, FF >: F](that: D1[K2, FF]): D1[K \/ K2, FF] =
        D1.either(this, that)

      /** If any keys match, everything is returned.
        * If no keys match, empty is returned.
        */
      final def filterHolistic(f: K => Boolean): D1[K, F] =
        if (isEmpty)
          this
        else {
          val anyKeysToMatch = iterator.exists(x => f(x._1))
          if (anyKeysToMatch)
            this
          else
            D1.empty
        }
    }

    object D1 {
      private[this] val _empty: D1[Any, Any] =
        new D1[Any, Any] {
          override def isEmpty = true
          override def apply(key: Any) = None
          override def mapKey[J](j: Intersection[Any, J]) = empty
          override def iterator = Iterator.empty
          override def keySet = Set.empty
        }

      def empty[K, F]: D1[K, F] =
        _empty.asInstanceOf[D1[K, F]]

      def apply[K, F](state: State.D1[K, F]): D1[K, F] =
        if (state.isEmpty)
          empty
        else
          mapped(state, Intersection.id)

      def mapped[SK, K, F](state: State.D1[SK, F], i: Intersection[SK, K]): D1[K, F] =
        if (state.isEmpty)
          empty
        else
          new D1[K, F] {
            override def isEmpty: Boolean =
              state.isEmpty

            override def apply(key: K): D0[F] =
              i.reverse.getOption(key).flatMap(state.get)

            override def mapKey[J](j: Intersection[K, J]): D1[J, F] =
              mapped(state, i <=> j)

            override def iterator: Iterator[(K, Status[F])] =
              i.reverse.id.fold(
                state.iterator.map(x => i.getOption(x._1) match {
                  case Some(k) => (k, x._2)
                  case None    => null
                }).filter(_ ne null)
              )(_.subst[λ[X => Iterator[(X, Status[F])]]](state.iterator))

            override def keySet: Set[K] =
              i.reverse.id.fold(
                state.keysIterator.map(i.getOption).filterDefined.toSet
              )(_.subst(state.keySet))
          }

      private[D1] def either[K1, K2, F](d1: D1[K1, F], d2: D1[K2, F]): D1[K1 \/ K2, F] =
        if (d1.isEmpty && d2.isEmpty)
          empty
        else
          new D1[K1 \/ K2, F] {
            override def isEmpty: Boolean =
              d1.isEmpty && d2.isEmpty

            override def apply(key: K1 \/ K2): D0[F] =
              key.fold(d1.apply, d2.apply)

            override def mapKey[J](j: Intersection[K1 \/ K2, J]): D1[J, F] =
              D1.mapped(iterator.toMap, j)

            override def iterator: Iterator[(K1 \/ K2, Status[F])] =
              d1.iterator.map(_.map1(-\/(_))) ++ d2.iterator.map(_.map1(\/-(_)))

            override def keySet: Set[K1 \/ K2] = {
              val s = Set.newBuilder[K1 \/ K2]
              s ++= d1.keySet.iterator.map(-\/(_))
              s ++= d2.keySet.iterator.map(\/-(_))
              s.result()
            }
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
      def withKey1(key: K1): D1[K2, F]
    }

    object D2 {
      def apply[K2, K1, F](state: State.D2[K2, K1, F]): D2[K2, K1, F] =
        mapped(state, Intersection.id, Intersection.id)

      def mapped[SK2, SK1, K2, K1, F](state: State.D2[SK2, SK1, F],
                                      i2: Intersection[SK2, K2],
                                      i1: Intersection[SK1, K1]): D2[K2, K1, F] =
        new D2[K2, K1, F] {
          override def isEmpty: Boolean =
            state.isEmpty

          override def apply(key: K2): D1[K1, F] =
            D1.mapped(i2.reverse.fold(key, state.get)(None).orEmptyMap, i1)

          override def mapKey2[J](j: Intersection[K2, J]): D2[J, K1, F] =
            mapped(state, i2 <=> j, i1)

          override def mapKey1[J](j: Intersection[K1, J]): D2[K2, J, F] =
            mapped(state, i2, i1 <=> j)

          override def iterator: Iterator[(K2, D1[K1, F])] =
            i2.reverse.id.fold(
              state.iterator
                .map(x => i2.fold(x._1, (_, D1.mapped(x._2, i1)))(null))
                .filter(_ ne null)
            )(_.subst[λ[X => Iterator[(X, State.D1[SK1, F])]]](state.iterator)
              .map(_.map2(D1.mapped(_, i1))))

          override def withKey1(k1: K1): D1[K2, F] = {
            // There's rarely going to be more than 1 async value in practice so this is most efficient
            var m: State.D1[K2, F] = Map.empty
            for {
              sk1       <- i1.reverse.getOption(k1)
              (sk2, s1) <- state
              s0        <- s1.get(sk1)
              k2        <- i2.getOption(sk2)
            } m = m.updated(k2, s0)
            D1(m)
          }
        }

      implicit def reusability[K2, K1, F]: Reusability[D2[K2, K1, F]] =
        Reusability.byRef || Reusability.when(_.isEmpty)
    }
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object Write {

    type D0[-F] = AsyncCall[F] ~=> Callback

    object D0 {
      def init[F]($: StateAccessPure[State.D0[F]]): D0[F] =
        apply(Reusable.fn.state($).set)

      def apply[F](setStateFn: State.D0[F] ~=> Callback): D0[F] =
        setStateFn.map(setState => call => {
          val clearStatus = setState(None)

          def onSuccess: Callback =
            clearStatus

          def onFailure: F => Callback =
            f => setState(Some(Status.Failed(f, Callback byName doIt, clearStatus)))

          lazy val doIt: Callback =
            // Switching this around breaks tests' MockServer's order of events.
            // i.e. it will call onSuccess which clears the status, and then set it to locked.
            setState(Some(Status.InProgress)) >> call(onSuccess, onFailure)

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
        def setBulk(ks: Iterable[K], value: => State.D0[F]): Callback
      }

      def apply[SK: UnivEq : ClassTag, K, F]($: Reusable[StateAccessPure[State.D1[SK, F]]],
                                             i: Intersection[SK, K]): D1[K, F] =
        // Doesn't factor Intersection into reusability because they're coherent
        $.map(_ => new Interface[K, F] {
          implicit val reusabilityK = Reusability.byUnivEq[SK]

          def lensAt(sk: SK) = Optics.mapValue[SK, Status[F]](sk)

          override def apply(k: K): D0[F] =
            i.reverse.fold[D0[F]](k,
              sk => D0(Reusable.ap($, Reusable.implicitly(sk))(($, sk) =>
                $.zoomStateL(lensAt(sk)).setState(_)))
            )(D0.doNothing)

          override def mapKey[J](j: Intersection[K, J]) =
            D1($, i <=> j)

          override def setBulk(ks: Iterable[K], _value: => State.D0[F]): Callback =
            Callback.unless(ks.isEmpty)(
              $.modState { initialState =>
                val value = _value
                ks.foldLeft(initialState)((s, k) =>
                  i.reverse.foldWarnFlip(k, s)(sk =>
                    lensAt(sk).set(value)(s))) })
        })

      def init[K: UnivEq : ClassTag, F]($: StateAccessPure[State.D1[K, F]]): D1[K, F] =
        apply(Reusable.byRef($), Intersection.id)

      def doNothing[K]: D1[K, Any] =
        withDoNothingReusability(new Interface[K, Any] {
          override def apply(k: K)                                   = D0.doNothing
          override def mapKey[J](j: Intersection[K, J])              = doNothing
          override def setBulk(ks: Iterable[K], v: => State.D0[Any]) = Callback(ks foreach Intersection.warnDiscard)
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
        def withKey1(key: K1): D1[K2, F]
      }

      def apply[SK2: UnivEq : ClassTag, SK1: UnivEq : ClassTag, K2, K1, F]($: Reusable[StateAccessPure[State.D2[SK2, SK1, F]]],
                                                                           i2: Intersection[SK2, K2],
                                                                           i1: Intersection[SK1, K1]): D2[K2, K1, F] =
        // Doesn't factor Intersections into reusability because they're coherent
        $.map(_ => new Interface[K2, K1, F] {
          implicit val reusabilityK2 = Reusability.byUnivEq[SK2]
          implicit val reusabilityK1 = Reusability.byUnivEq[SK1]

          def lensAt(sk2: SK2) = Optics.innerMap[SK2, SK1, Status[F]](sk2)

          override def apply(k: K2): D1[K1, F] =
            i2.reverse.foldFlip[D1[K1, F]](k, D1.doNothing)(sk =>
              D1(Reusable.ap($, Reusable.implicitly(sk))(($, sk) =>
                $.zoomStateL(lensAt(sk))), i1))

          override def mapKey2[J](i: Intersection[K2, J]): D2[J, K1, F] =
            D2($, i2 <=> i, i1)

          override def mapKey1[J](i: Intersection[K1, J]): D2[K2, J, F] =
            D2($, i2, i1 <=> i)

          override def setBulk(k2s: Iterable[K2], k1: K1, value: => State.D0[F]): Callback =
            Callback.unless(k2s.isEmpty)(
              i1.reverse.foldWarnFlip(k1, Callback.empty)(sk1 =>
                $.modState { initialState =>
                  val setValue = value.toMapEntrySetFn[SK1]
                  k2s.foldLeft(initialState)((s, k2) =>
                    i2.reverse.foldWarnFlip(k2, s)(sk2 =>
                      lensAt(sk2).modify(setValue(_, sk1))(s) ))}))

          override def withKey1(k1: K1): D1[K2, F] =
            i1.reverse.getOption(k1) match {
              case Some(sk1) =>
                val self = this
                Reusable.ap($, Reusable.implicitly(sk1))((_, _) =>
                  new D1.Interface[K2, F] {
                    override def apply(k2: K2)                                = self(k2)(k1)
                    override def mapKey[J](j: Intersection[K2, J])            = self.mapKey2(j).withKey1(k1)
                    override def setBulk(ks: Iterable[K2], v: => State.D0[F]) = self.setBulk(ks, k1, v)
                  }
                )
              case None => D1.doNothing
            }
        })

      def init[K2: UnivEq : ClassTag, K1: UnivEq : ClassTag, F]($: StateAccessPure[State.D2[K2, K1, F]]): D2[K2, K1, F] =
        apply(Reusable.byRef($), Intersection.id, Intersection.id)
    }

  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object ReadWrite {

    final case class D0[F](write: Write.D0[F], read: Read.D0[F])

    final case class D1[K, F](write: Write.D1[K, F], read: Read.D1[K, F]) {
      def apply(k: K): D0[F] =
        D0(write(k), read(k))

      def mapKey[J](i: Intersection[K, J]): D1[J, F] =
        D1(write.mapKey(i), read.mapKey(i))
    }

    final case class D2[K2, K1, F](write: Write.D2[K2, K1, F], read: Read.D2[K2, K1, F]) {
      def apply(k: K2): D1[K1, F] =
        D1(write(k), read(k))

      def mapKey2[J](i: Intersection[K2, J]): D2[J, K1, F] =
        D2(write.mapKey2(i), read.mapKey2(i))

      def mapKey1[J](i: Intersection[K1, J]): D2[K2, J, F] =
        D2(write.mapKey1(i), read.mapKey1(i))

      def withKey1(k: K1): D1[K2, F] =
        D1(write.withKey1(k), read.withKey1(k))
    }

    implicit def reusabilityD0[F]        : Reusability[D0[F]]         = Reusability.derive
    implicit def reusabilityD1[K, F]     : Reusability[D1[K, F]]      = Reusability.derive
    implicit def reusabilityD2[K2, K1, F]: Reusability[D2[K2, K1, F]] = Reusability.derive
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  /** It's extremely common to pass around async state and a `I ~=> Callback` already wired with async write logic.
    *
    * Because both data are related by async key, it's safer to tie them together at the type level rather than by
    * field names.
    */
  object Runner {

    final case class D0[-I, +F](asyncState: Read.D0[F], run: I ~=> Callback) {
      @inline def runnable: Boolean =
        asyncState.isEmpty

      def runOption(i: I): Option[Callback] =
        if (runnable) Some(run(i)) else None

      def runOrDoNothing(i: I): Callback =
        if (runnable) run(i) else Callback.empty

//      def contramapNoReuse[A](f: A => I): D0[A, F] =
//        D0(asyncState, Reusable.never(run.value compose f))
//
//      def toD0O: D0O[I, F] =
//        D0O(asyncState, run.map(_.andThen(Some(_))))

      def mapRunOption[I2](f: (I ~=> Callback) => (I2 ~=> Option[Callback])): D0O[I2, F] =
        D0O(asyncState, f(run))
    }

    /** D0 + O for Option. run becomes tryRun with the result being Option[Callback] */
    final case class D0O[-I, +F](asyncState: Read.D0[F], tryRun: I ~=> Option[Callback]) {
      @inline def runnable: Boolean =
        asyncState.isEmpty

      def runOption(i: I): Option[Callback] =
        if (runnable) tryRun(i) else None

      def runOrDoNothing(i: I): Callback =
        if (runnable) tryRun(i).getOrEmpty else Callback.empty

//      def contramapNoReuse[A](f: A => I): D0O[A, F] =
//        D0O(asyncState, Reusable.never(tryRun.value compose f))
    }

    final case class D1[K, -I, +F](asyncState: Read.D1[K, F], run: K ~=> (I ~=> Callback)) {
      def apply(k: K): D0[I, F] =
        D0(asyncState(k), run(k))
    }

    private val reusabilityAny0 = Reusability.derive[D0[Nothing, Any]]
    private val reusabilityAny1 = Reusability.derive[D1[Any, Nothing, Any]]
    implicit def reusabilityD0   [I, F]: Reusability[D0   [I, F]] = reusabilityAny0.narrow
    implicit def reusabilityD1[K, I, F]: Reusability[D1[K, I, F]] = reusabilityAny1.asInstanceOf[Reusability[D1[K, I, F]]]
  }

}
