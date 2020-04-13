package shipreq.benchmark

import japgolly.microlibs.recursion._
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scalaz.Functor

object PickleFixBM {
  import boopickle.DefaultBasic._

  object FixpointPickler1 {
    def pickleFix[F[_]: Functor](implicit p: Pickler[F[Unit]]): Pickler[Fix[F]] =
      new Pickler[Fix[F]] {
        override def pickle(f: Fix[F])(implicit state: PickleState): Unit = {
          val fUnit = Functor[F].void(f.unfix)
          p.pickle(fUnit)
          Functor[F].map(f.unfix)(pickle)
          ()
        }

        override def unpickle(implicit state: UnpickleState) = {
          val fUnit = p.unpickle
          Fix(Functor[F].map(fUnit)(_ => unpickle))
        }
    }
  }

  object FixpointPickler2 {
    def pickleFix[F[_]: Functor](implicit p: Pickler[F[Unit]]): Pickler[Fix[F]] =
      new Pickler[Fix[F]] {
        override def pickle(f: Fix[F])(implicit state: PickleState): Unit = {
          // Uses stack
          var fields: () => Unit = () => ()
          val fUnit = Functor[F].map(f.unfix)(a => {
            val head = fields
            fields = () => {head(); pickle(a)}
          })
          p.pickle(fUnit)
          fields()
        }

        override def unpickle(implicit state: UnpickleState) = {
          val fUnit = p.unpickle
          Fix(Functor[F].map(fUnit)(_ => unpickle))
        }
    }
  }

  object FixpointPickler3 {
    def pickleFix[F[_]: Functor](implicit p: Pickler[F[Unit]]): Pickler[Fix[F]] =
      new Pickler[Fix[F]] {
        override def pickle(f: Fix[F])(implicit state: PickleState): Unit = {
          val fields = new collection.mutable.ArrayBuffer[Fix[F]](64)
          val fUnit = Functor[F].map(f.unfix)(a => {
            fields += a
            ()
          })
          p.pickle(fUnit)
          fields.foreach(pickle)
          ()
        }

        override def unpickle(implicit state: UnpickleState) = {
          val fUnit = p.unpickle
          Fix(Functor[F].map(fUnit)(_ => unpickle))
        }
    }
  }

  sealed trait CalcF[F]
  case class Num[F](n: Char)             extends CalcF[F]
  case class Neg[F](f: F)                extends CalcF[F]
  case class Add[F](f: F, fs: Vector[F]) extends CalcF[F]

  implicit val functorCalcF: Functor[CalcF] = new Functor[CalcF] {
    override def map[A, B](fa: CalcF[A])(f: A => B) = fa match {
      case Num(n)     => Num(n)
      case Neg(a)     => Neg(f(a))
      case Add(a, as) => Add(f(a), as map f)
    }
  }

  type Calc = Fix[CalcF]
  def calc(f: CalcF[Calc]): Calc = Fix(f)
  def chr(n: Char): Calc = calc(Num(n))
  def neg(f: Calc): Calc = calc(Neg(f))
  def add(f: Calc, fs: Calc*): Calc = calc(Add(f, fs.toVector))

  private implicit val picklerNum: Pickler[Num[Unit]] =
    transformPickler(Num.apply[Unit])(_.n)

  private implicit val picklerNeg: Pickler[Neg[Unit]] =
    transformPickler(Neg.apply[Unit])(_.f)

  private implicit val picklerAdd: Pickler[Add[Unit]] =
    new Pickler[Add[Unit]] {
      override def pickle(a: Add[Unit])(implicit state: PickleState): Unit = {
        state.pickle(a.f)
        state.pickle(a.fs)
      }
      override def unpickle(implicit state: UnpickleState): Add[Unit] = {
        val f  = state.unpickle[Unit]
        val fs = state.unpickle[Vector[Unit]]
        Add(f, fs)
      }
    }

  private implicit val picklerCalcF: Pickler[CalcF[Unit]] =
    new Pickler[CalcF[Unit]] {
      override def pickle(a: CalcF[Unit])(implicit state: PickleState): Unit =
        a match {
          case b: Add[Unit] => state.enc.writeByte(0); state.pickle(b)
          case b: Neg[Unit] => state.enc.writeByte(1); state.pickle(b)
          case b: Num[Unit] => state.enc.writeByte(2); state.pickle(b)
        }
      override def unpickle(implicit state: UnpickleState): CalcF[Unit] =
        state.dec.readByte match {
          case 0 => state.unpickle[Add[Unit]]
          case 1 => state.unpickle[Neg[Unit]]
          case 2 => state.unpickle[Num[Unit]]
        }
    }

  val p1: Pickler[Calc] = FixpointPickler1.pickleFix
  val p2: Pickler[Calc] = FixpointPickler2.pickleFix
  val p3: Pickler[Calc] = FixpointPickler3.pickleFix

  case class Spec(width: Int, depth: Int)
  val gen: FCoalgebra[CalcF, Spec] = {
    case Spec(_, 0) => Num('A')
    case Spec(w, d) => val s = Spec(w, d - 1); Add(s, Vector.fill(w - 1)(s))
  }
}

@Warmup(iterations = 10)
@Measurement(iterations = 10)
@Fork(1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class PickleFixBM {
  import boopickle.PickleImpl
  import PickleFixBM.{Calc, gen, Spec}

  @Param(Array("2", "6"))
  var width: Int = _

  @Param(Array("2", "6"))
  var depth: Int = _

  var data: Calc = _

  @Setup def setup() = {
    data = Recursion.ana(gen)(Spec(width, depth))
  }

//  @Benchmark def test1 = {
//    implicit val p = PickleFixBM.p1
//    PickleImpl intoBytes data
//  }
//
//  @Benchmark def test2 = {
//    implicit val p = PickleFixBM.p2
//    PickleImpl intoBytes data
//  }

  @Benchmark def test3 = {
    implicit val p = PickleFixBM.p3
    PickleImpl intoBytes data
  }
}
