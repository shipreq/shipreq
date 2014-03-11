package shipreq.taskman.api

import scalaz.{Coyoneda, Free, ~>}
import scalaz.Scalaz.Id
import scalaz.effect.IO
import scalaz.Free.FreeC

object Task2 {

  trait TaskDef
  sealed trait Task[A]
  case class Create(t: Seq[TaskDef]) extends Task[Unit]

  implicit object TaskImpl extends (Task ~> Id) {
    override def apply[A](ta: Task[A]): Id[A] = ta match {
      case Create(t) => { println(s"I got: $t !") }
    }
  }

  implicit object TaskToIO extends (Task ~> IO) {
    override def apply[A](ta: Task[A]): IO[A] = ta match {
      case Create(n) => IO{ println(s"hello $n") }
    }
  }

  type FreeTask[A] = Free.FreeC[Task, A]

  implicit def TaskToFreeTask[A](t: Task[A]): FreeTask[A] = {
    val c: Coyoneda[Task, A] = Coyoneda(t) // TODO pending scalaz patch
    Free.liftFU(c)
  }

  val taskDef: TaskDef = new TaskDef {override def toString = "TASKDEF"}
  val freeTask: FreeTask[Unit] = Create(Seq(taskDef)) >>= (_ => Create(Seq(taskDef, taskDef)))

  /*
    //  implicit object TaskFunctionByCoyoneda extends Functor[Task] {
  //    override def map[A, B](ta: Task[A])(f: A => B): Task[B] =
  //      Coyoneda(ta).map(f).tr
  //  }

  */

//  final def interpret[M[_], N[_], A](free: FreeC[N, A])(f: N ~> M)(implicit M: Monad[M]): M[A] = {
//    def go(a: FreeC[N, A]): M[A] = a.resume match {
//      case \/-(c) => M.point(c)
//      case -\/(c) => M.bind(f(c.fi))(x => go(c.k(x)))
//    }
//    go(free)
//  }

  def mapSuspensionFreeC[F[_], G[_], A](c: FreeC[F, A], f: F ~> G): FreeC[G, A] = {
    type CoyonedaG[A] = Coyoneda[G, A]
    c.mapSuspension[CoyonedaG](new (({type λ[α] = Coyoneda[F, α]})#λ ~> CoyonedaG){
      def apply[A](a: Coyoneda[F, A]) = a.trans(f)
    })
  }

//  implicit object TaskCToIO extends (({type L[x] = Coyoneda[Task, x]})#L ~> IO) {
//    override def apply[A](c: ({type L[x] = Coyoneda[Task, x]})#L[A]): IO[A] =
//      c.fi.toIO
//  }

  val freeCIO: FreeC[IO, Unit] = mapSuspensionFreeC(freeTask, TaskToIO)

//  val ioFunctor = implicitly[Functor[IO]]
  object CIOToIO extends (({type L[x] = Coyoneda[IO, x]})#L ~> IO) {
    override def apply[A](m: ({type L[x] = Coyoneda[IO, x]})#L[A]): IO[A] = m.run
  }

  val freeio: Free[IO, Unit] = freeCIO.mapSuspension(CIOToIO)

  def main(args: Array[String]): Unit = {
    val main: IO[Unit] = freeio.runM(identity)
    main.unsafePerformIO()
  }
}