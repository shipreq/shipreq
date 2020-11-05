package shipreq.webapp.server.redis

import io.circe._
import scala.reflect.ClassTag
import scalaz.{Equal, Monad, Semigroup}
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.server.logic.algebra.Redis._

trait RedisLaw[Input] {
  import RedisLaw._

  type Output

  val name            : String
  val equation        : Input => Equation[Output]
  val inputJsonDecoder: Decoder[Input]
  val inputJsonEncoder: Encoder[Input]
}

object RedisLaw {

  type Aux[I, O] = RedisLaw[I] { type Output = O }

  def apply[I: Decoder: Encoder, O](name: String, equation: I => Equation[O]): Aux[I, O] = {
    val _name   = name
    val _equation = equation
    new RedisLaw[I] {
      override type Output          = O
      override val name             = _name
      override val equation         = _equation
      override val inputJsonDecoder = implicitly
      override val inputJsonEncoder = implicitly
    }
  }

  def apply[I](name: String) =
    new Dsl[I](name)

  final class Dsl[I](private val name: String) extends AnyVal {
    def apply[O](equation: I => Equation[O])(implicit d: Decoder[I], e: Encoder[I]): Aux[I, O] =
      RedisLaw(name, equation)
  }

  // ===================================================================================================================

  private implicit val equalUnit: Equal[Unit] =
    Equal((_, _) => true)

  final case class Logic[A](run: (ProjectAlgebra[Fx], ProjectId) => Fx[A]) {

    def map[B](f: A => B): Logic[B] =
      Logic(run(_, _).map(f))

    def void: Logic[Unit] =
      map(_ => ())

    def flatMap[B](f: A => Logic[B]): Logic[B] =
      Logic((x, y) => run(x, y).flatMap(f(_).run(x, y)))

    def ++(fb: Logic[A])(implicit A: Semigroup[A]): Logic[A] =
      Logic((x, y) =>
        for {
          a <- run(x, y)
          b <- fb.run(x, y)
        } yield A.append(a, b)
      )

    def ===(fb: Logic[A])(implicit e: Equal[A]): Equation[A] =
      Equation(this, fb, e)

    def <->(fb: Logic[A]): Equation[Unit] =
      this.void === fb.void
  }

  object Logic {

    implicit val monadLogic: Monad[Logic] = new Monad[Logic] {
      override def point[A](a: => A): Logic[A] = {
        val fx = Fx(a)
        Logic((_, _) => fx)
      }

      override def map[A, B](fa: Logic[A])(f: A => B): Logic[B] =
        fa.map(f)

      override def bind[A, B](fa: Logic[A])(f: A => Logic[B]): Logic[B] =
        fa.flatMap(f)
    }
  }

  final case class Equation[O](lhs     : Logic[O],
                               rhs     : Logic[O],
                               equality: Equal[O])

  // ===================================================================================================================

  trait Test {
    type Input
    val law: RedisLaw[Input]
    val input: Input

    final lazy val equation = law.equation(input)
    final def name          = law.name
    final def inputJson     = law.inputJsonEncoder(input)

    def withInput(i: Input): Test.Aux[Input] =
      Test(law)(i)

    def castAttempt[A](implicit ct: ClassTag[A]): Option[Test.Aux[A]] =
      Option.when(ct.runtimeClass.isInstance(input))(this.asInstanceOf[Test.Aux[A]])

    def castAttempt2[A, B](implicit a: ClassTag[A], b: ClassTag[A]): Option[Test.Aux[(A, B)]] =
      Option.when(
        input match {
          case t: (Any, Any) =>
            a.runtimeClass.isInstance(t._1) &&
            b.runtimeClass.isInstance(t._2)
          case _ => false
        }
      )(this.asInstanceOf[Test.Aux[(A, B)]])

    def castAttempt4[A, B, C, D](implicit a: ClassTag[A], b: ClassTag[A], c: ClassTag[A], d: ClassTag[A]): Option[Test.Aux[(A, B, C, D)]] =
      Option.when(
        input match {
          case t: (Any, Any, Any, Any) =>
            a.runtimeClass.isInstance(t._1) &&
            b.runtimeClass.isInstance(t._2) &&
            c.runtimeClass.isInstance(t._3) &&
            d.runtimeClass.isInstance(t._4)
          case _ => false
        }
      )(this.asInstanceOf[Test.Aux[(A, B, C, D)]])
  }

  object Test {
    type Aux[I] = Test { type Input = I }

    def apply[I](law  : RedisLaw[I])
                (input: I): Aux[I] = {
      val _law   = law
      val _input = input
      new Test {
        override type Input         = I
        override val law: _law.type = _law
        override val input          = _input
      }
    }
  }

}
