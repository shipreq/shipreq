package shipreq.taskman.api

import scalaz.{Coyoneda, Free, ~>, Functor, \/, -\/}
import scalaz.Scalaz.Id
import scalaz.effect.IO
import scalaz.Free.FreeC
import scalaz.std.function.function0Instance

import ScalazExt._
import shipreq.base.util.TypeTags
//  import scalaz._, Scalaz._
//  import scalaz.syntax.functor._
//  import scalaz.std.list._

// TODO Merge with webapp's types

object Types extends Types {
  sealed trait IsUserId extends TypeTag[JLong]
  sealed trait IsEmailAddr extends TypeTag[String]
}

trait Types extends TypeTags {
  import Types._
  type UserId = JLong @@ IsUserId
  type EmailAddr = String @@ IsEmailAddr
}
import Types._

// --------------------------------------------------------------------------------------------------------------------

//object Main{
//  def main(a: Array[String]) {
//    println(TaskTypes.ById(102))
//  }
//}

// --------------------------------------------------------------------------------------------------------------------

object TaskmanApi {
  trait Cmd[A]
  type CmdF[A] = FreeC[Cmd, A]
  implicit def cmdLiftF[A](c: Cmd[A]): CmdF[A] = liftFC(c)

  case class SubmitTask1(w: TaskDef) extends Cmd[Unit]
  case class SubmitTask(w: Seq[TaskDef]) extends Cmd[Unit]
}

object Effect {

  type IOM[A] = Function0[A]

  def iom[A](a: => A): IOM[A] = () => a

  def compile[C[_], A](f: FreeC[C, A], t: C ~> IOM): IO[A] = {
    val g = f.mapSuspension(FG_to_CFG(t))
    IO{ g.run }
  }
}
