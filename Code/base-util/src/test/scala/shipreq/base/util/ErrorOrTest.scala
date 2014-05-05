package shipreq.base.util

import java.util.concurrent.atomic.AtomicBoolean
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.{Gen, Arbitrary}
import scalaz.Equal
import scalaz.effect.IO
import scalaz.scalacheck.ScalazProperties
import scalaz.std.list._
import ErrorOr.Implicits._
import Arbitrary._

class ErrorOrTest extends Specification with ScalaCheck {

  val e = Error apply "!"
  val eo = ErrorOr error "!"

  type LE[A] = List[ErrorOr[A]]
  implicit def EQ[T] = Equal.equalA[ErrorOr[T]]
  implicit val LEM = ErrorOr.Scalaz.monadInstance[List]

  implicit def arbErrorOr[T](implicit t: Arbitrary[T]) = Arbitrary[ErrorOr[T]] {
    Gen.oneOf(t.arbitrary map ErrorOr.apply, Gen.const(eo))
  }
  implicit def ALE[T](implicit t: Arbitrary[ErrorOr[T]]): Arbitrary[LE[T]] = Arbitrary(Gen.listOf(t.arbitrary))
  implicit def AII: Arbitrary[Int => Int] = Arbitrary(Gen.oneOf((_: Int) + 100, (_: Int) << 1))

  "Scalaz typeclasses" >> {
    "Monad laws" ! check(ScalazProperties.monad.laws[LE])
  }

  "MonadExt" >> {
    def once[A](a: => A): IO[A] = {
      var first = true
      IO(if (first) {first = false; a} else sys.error("More than once"))
    }

    def once2[A](a: => A): (AtomicBoolean, IO[A]) = {
      val called = new AtomicBoolean(false)
      val io = once{ called.set(true); a }
      (called, io)
    }

    "map" ! prop { (a: ErrorOr[Int]) =>
      once(a)._mapE(6).unsafePerformIO == a.map(_ => 6)
    }

    "emap" ! prop { (a: ErrorOr[Int], b: ErrorOr[Boolean]) =>
      once(a)._emapE(b).unsafePerformIO == a.flatMap(_ => b)
    }

    "fmap" ! prop { (a: ErrorOr[Int], b: ErrorOr[Boolean]) =>
      once(a)._fmapE(once(b)).unsafePerformIO == a.flatMap(_ => b)
    }

    "tap" ! prop { (a: ErrorOr[Int]) =>
      val (called, tap) = once2(())
      val r = once(a).tapE(_ => tap).unsafePerformIO
      (called.get() == a.isRight) :| "Effect" && (r == a) :| "Result"
    }

    "ftap" ! prop { (a: ErrorOr[Int]) =>
      val (called, tap) = once2(ErrorOr.unit)
      val r = once(a).ftapE(_ => tap).unsafePerformIO
      (called.get() == a.isRight) :| "Effect" && (r == a) :| "Result"
    }

    "exec" ! prop { (a: ErrorOr[Int]) =>
      val (aCalled, aa) = once2(a)
      val (errIoCalled, errIo) = once2(())
      aa.execE(_ => errIo).unsafePerformIO
      aCalled.get() :| "A" && (errIoCalled.get() == a.isLeft) :| "Error"
    }
  }

  "Error" >> {
    "choose" ! prop { (a: ErrorOr[Int]) =>
      Error.choose(a, e).swap.toOption == Some(a.swap.toOption getOrElse e)
    }
  }
}
