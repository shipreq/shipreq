package shipreq.base.util

import scalaz.{Monad, -\/, \/-, \/}
import StateEither.{Failure, Ok, Result}

/**
 * State monad + either monad stack.
 *
 * s => e \/ (s, a)
 */
final class StateEither[S, E, +A](val run: S => Result[S, E, A]) extends AnyVal {

  def exec(initialState: S): E \/ S =
    run(initialState) match {
      case Ok(s, _)   => \/-(s)
      case Failure(e) => -\/(e)
    }

  def eval(initialState: S): E \/ A =
    run(initialState) match {
      case Ok(_, a)   => \/-(a)
      case Failure(e) => -\/(e)
    }

  def map[B](f: A => B): StateEither[S, E, B] =
    StateEither(run(_) map f)

  /** Sequence a callback to run after this, discarding any value produced by this. */
  @inline def >>[B](f: StateEither[S, E, B]): StateEither[S, E, B] =
    flatMap(_ => f)

  /** Sequence a callback to run before this, discarding any value produced by it. */
  @inline def <<[B](f: StateEither[S, E, B]): StateEither[S, E, A] =
    f >> this

  def flatMap[B](f: A => StateEither[S, E, B]): StateEither[S, E, B] =
    StateEither(s1 => run(s1).flatMap2((s2, a) => f(a).run(s2)))

  /** Sequence actions, discarding the value of the second argument. */
  def flatTap[B](f: A => StateEither[S, E, B]): StateEither[S, E, A] =
    for {a <- this; _ <- f(a)} yield a

  /** Sequence actions, discarding the value of the second argument. */
  @inline def <*[B](next: StateEither[S, E, B]): StateEither[S, E, A] =
    flatTap(_ => next)

  def tap[B](f: A => B): StateEither[S, E, A] =
    this <* map(f)

  /**
   * Return a value.
   *
   * @param b is strict to optimise the success path.
   */
  def ret[B](b: B): StateEither[S, E, B] =
    map(_ => b)

  def mapE[B](f: A => E \/ B): StateEither[S, E, B] =
    StateEither(run(_).flatMap2((s, a) => f(a).fold(Failure(_), Ok(s, _))))

  def mapResult[B](f: A => Result[S, E, B]): StateEither[S, E, B] =
    StateEither(run(_) flatMap f)

  def mapResult2[B](f: (S, A) => Result[S, E, B]): StateEither[S, E, B] =
    StateEither(run(_) flatMap2 f)

  def setResult[B](r: Result[S, E, B]): StateEither[S, E, B] =
    StateEither(run(_) >> r)

  def setState(s: S): StateEither[S, E, Unit] =
    setResult(Ok(s, ()))

  def void: StateEither[S, E, Unit] =
    ret(())

  def attempt(e: Throwable => E): StateEither[S, E, A] =
    StateEither(s =>
      try run(s)
      catch { case t: Throwable => Failure(e(t)) })

  def foldMapBind[AA >: A, B](bs: TraversableOnce[B])(f: B => AA => StateEither[S, E, AA]): StateEither[S, E, AA] =
    bs.foldLeft(this: StateEither[S, E, AA])(_ >>= f(_))

  def leftMap[E2](f: E => E2): StateEither[S, E2, A] =
    StateEither(s1 => run(s1).leftMap(f))

  def improveFailure[E2, E3, B](onFailure: => StateEither[S, E2, B])
                               (mergeResults: (E, E2 \/ (S, B)) => E3): StateEither[S, E3, A] =
    StateEither(s1 => run(s1).leftMap(e => mergeResults(e, onFailure.run(s1).toDisj)))

  /** Alias for `map`. */
  @inline def |>[B](f: A => B): StateEither[S, E, B] =
    map(f)

  /** Alias for `tap`. */
  @inline def <|[B](f: A => B): StateEither[S, E, A] =
    tap(f)

  /** Alias for `flatMap`. */
  @inline def >>=[B](f: A => StateEither[S, E, B]): StateEither[S, E, B] =
    flatMap(f)

  /** Alias for `flatTap`. */
  @inline def <*=[B](f: A => StateEither[S, E, B]): StateEither[S, E, A] =
    flatTap(f)

  /** Alias for `ret`. */
  @inline def |>>[B](b: B): StateEither[S, E, B] =
    ret(b)
}

// =====================================================================================================================
object StateEither {

  @inline def apply[S, E, A](run: S => Result[S, E, A]): StateEither[S, E, A] =
    new StateEither(run)

  def feed[S, E, A](f: S => StateEither[S, E, A]): StateEither[S, E, A] =
    new StateEither(s => f(s).run(s))

  def point[S, E, A](a: => A): StateEither[S, E, A] =
    StateEither(s => Ok(s, a))

  def ret[S, E, A](a: A): StateEither[S, E, A] =
    StateEither(Ok(_, a))

  def fail[S, E](e: E): StateEither[S, E, Nothing] =
    StateEither(_ => Failure(e))

  def retOption[S, E, A](o: Option[A], e: => E): StateEither[S, E, A] =
    o.fold[StateEither[S, E, A]](fail(e))(ret)

  def liftOption[S, E, A](o: Option[StateEither[S, E, A]]): StateEither[S, E, Option[A]] =
    o.fold[StateEither[S, E, Option[A]]](ret(None))(_ map Some.apply)

  def get[S, E, A](f: S => A): StateEither[S, E, A] =
    StateEither(s => Ok(s, f(s)))

  def getE[S, E, A](f: S => E \/ A): StateEither[S, E, A] =
    StateEither(s => f(s).fold(Failure(_), Ok(s, _)))

  def mod[S, E](f: S => S): StateEither[S, E, Unit] =
    StateEither(s => Ok(f(s), ()))

  def modE[S, E](f: S => E \/ S): StateEither[S, E, Unit] =
    StateEither(s => f(s).fold(Failure(_), Ok(_, ())))

  def test[S, E, A](f: S => Boolean, whenFalse: => E): StateEither[S, E, Unit] =
    StateEither(s => if (f(s)) Ok(s, ()) else Failure(whenFalse))

  def testO[S, E, A](f: S => Option[E]): StateEither[S, E, Unit] =
    StateEither(s => f(s).fold[Result[S, E, Unit]](Ok(s, ()))(Failure(_)))

  def foldMapRun[S, E, A](as: Iterable[A])(f: A => StateEither[S, E, Unit]): StateEither[S, E, Unit] =
    // as.foldLeft(ret[S, E, Unit](()))(_ >> f(_))
    Util.mapReduce(as, ret[S, E, Unit](()))(f, _ >> _)

  // ===================================================================================================================
  def Fix[S, E] = new Fix[S, E]

  final class Fix[S, E] {
    type SE    [+A] = StateEither[S, E, A]
    type Result[+A] = StateEither.Result[S, E, A]
    type Ok    [+A] = StateEither.Ok[S, A]
    type Failure    = StateEither.Failure[E]

    @inline def Failure(failure: E): Failure =
      StateEither.Failure(failure)

    @inline def Ok[A](state: S, value: A): Ok[A] =
      StateEither.Ok(state, value)

    val get : SE[S]           = get(identity)
    val nop : SE[Unit]        = ret(())
    val _nop: Any => SE[Unit] = Function const nop

    def test(f: Boolean, whenFalse: => E): SE[Unit] =
      if (f) nop else fail(whenFalse)

    @inline def apply    [A](run: S => Result[A])             : SE[A]       = StateEither(run)
    @inline def feed     [A](f: S => SE[A])                   : SE[A]       = StateEither feed f
    @inline def ret      [A](a: A)                            : SE[A]       = StateEither ret a
    @inline def point    [A](a: => A)                         : SE[A]       = StateEither point a
    @inline def fail        (e: E)                            : SE[Nothing] = StateEither fail e
    @inline def mod         (f: S => S)                       : SE[Unit]    = StateEither mod f
    @inline def modE        (f: S => E \/ S)                  : SE[Unit]    = StateEither modE f
    @inline def test        (f: S => Boolean, whenFalse: => E): SE[Unit]    = StateEither.test(f, whenFalse)
    @inline def testO       (f: S => Option[E])               : SE[Unit]    = StateEither testO f
    @inline def get      [A](f: S => A)                       : SE[A]       = StateEither get f
    @inline def getE     [A](f: S => E \/ A)                  : SE[A]       = StateEither getE f
    @inline def retOption[A](o: Option[A], e: => E)           : SE[A]       = StateEither.retOption(o, e)

    def foldMapRun[A](as: TraversableOnce[A])(f: A => SE[Unit]): SE[Unit] =
      // as.foldLeft(nop)(_ >> f(_))
      Util.mapReduce(as, nop)(f, _ >> _)

    implicit val monadSE: Monad[SE] =
      new Monad[SE] {
        override def point[A]   (a: => A)                 : SE[A] = get(_ => a)
        override def bind [A, B](fa: SE[A])(f: A => SE[B]): SE[B] = fa >>= f
        override def map  [A, B](fa: SE[A])(f: A => B)    : SE[B] = fa map f
      }
  }

  // ===================================================================================================================
  sealed abstract class Result[+S, +E, +A] {
    def map     [B            ](f: A => B                   ): Result[S, E, B]
    def leftMap [B            ](f: E => B                   ): Result[S, B, A]
    def flatMap [T>:S, F>:E, B](f: A => Result[T, F, B]     ): Result[T, F, B]
    def flatMap2[T>:S, F>:E, B](f: (S, A) => Result[T, F, B]): Result[T, F, B]
    def >>      [T>:S, F>:E, B](f: Result[T, F, B]          ): Result[T, F, B]
    def toDisj                                               : E \/ (S, A)

    final def flatTap[T>:S, F>:E, B](f: A => Result[T, F, B]): Result[T, F, A] =
      for {a <- this; _ <- f(a)} yield a

    /** Sequence a callback to run before this, discarding any value produced by it. */
    @inline final def <<[T>:S, F>:E, B](f: Result[T, F, B]): Result[T, F, A] =
      f >> this

    /** Sequence actions, discarding the value of the second argument. */
    @inline final def <*[T>:S, F>:E, B](next: Result[T, F, B]): Result[T, F, A] =
      flatTap(_ => next)
  }

  final case class Failure[+E](failure: E) extends Result[Nothing, E, Nothing] {
    type S = Nothing
    type A = Nothing
    override def map     [B            ](f: A => B                   ) = this
    override def leftMap [B            ](f: E => B                   ) = Failure(f(failure))
    override def flatMap [T>:S, F>:E, B](f: A => Result[T, F, B]     ) = this
    override def flatMap2[T>:S, F>:E, B](f: (S, A) => Result[T, F, B]) = this
    override def >>      [T>:S, F>:E, B](f: Result[T, F, B]          ) = this
    override def toDisj                                                = -\/(failure)
  }

  final case class Ok[+S, +A](state: S, value: A) extends Result[S, Nothing, A] {
    type E = Nothing
    override def map     [B            ](f: A => B                   ) = Ok(state, f(value))
    override def leftMap [B            ](f: E => B                   ) = this
    override def flatMap [T>:S, F>:E, B](f: A => Result[T, F, B]     ) = f(value)
    override def flatMap2[T>:S, F>:E, B](f: (S, A) => Result[T, F, B]) = f(state, value)
    override def >>      [T>:S, F>:E, B](f: Result[T, F, B]          ) = f
    override def toDisj                                                = \/-((state, value))
  }
}