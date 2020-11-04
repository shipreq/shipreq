package shipreq.webapp.base.util

import japgolly.scalajs.react._

/**
 * Typed Callback. A [[Callback]] attached with type to indicate and distinguish purpose.
 */
final class TCB[T <: TCB.Type] private[TCB] (private val f: () => Unit) extends AnyVal {
  type Self = TCB[T]

  @inline def cb: Callback =
    Callback.lift(f)

  @inline def >>(next: Callback): Self =
    new TCB(cb.flatMap(_ => next).toScalaFn)

  @inline def <<(prev: Callback): Self =
    new TCB(prev.flatMap(_ => cb).toScalaFn)
}

object TCB {
  sealed trait Type

  @inline implicit def autoResolveToCB(c: TCB[_ <: TCB.Type]): Callback =
    c.cb

  final class Ctor[T <: Type] private[TCB]() {

    @inline def apply(cb: Callback): TCB[T] =
      new TCB(cb.toScalaFn)

    val nop: TCB[T] =
      apply(Callback.empty)

    val _nop: Any => TCB[T] =
      _ => nop

    def lazily(f: => Callback) =
      apply(Callback lazily f)
  }

  // ----------------------------------
  // Types

  sealed trait TypeSuccess extends Type
  type Success = TCB[TypeSuccess]
  val Success = new Ctor[TypeSuccess]()

  sealed trait TypeFailure extends Type
  type Failure = TCB[TypeFailure]
  val Failure = new Ctor[TypeFailure]()

  sealed trait TypeAbort extends Type
  type Abort = TCB[TypeAbort]
  val Abort = new Ctor[TypeAbort]()

  sealed trait TypeCommit extends Type
  type Commit = TCB[TypeCommit]
  val Commit = new Ctor[TypeCommit]()

  /**
   * Something to be executed at the end of a process, regardless of its outcome.
   */
  sealed trait TypeFinally extends Type
  type Finally = TCB[TypeFinally]
  val Finally = new Ctor[TypeFinally]()
}