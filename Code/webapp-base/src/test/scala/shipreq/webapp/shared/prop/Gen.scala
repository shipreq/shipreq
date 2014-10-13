package shipreq.webapp.shared.prop

import com.nicta.rng.{Rng, Size}
import scalaz._
import shipreq.base.util.TaggedTypes.TaggedInt

final case class GenSize(value: Int) extends TaggedInt
final case class SampleSize(value: Int) extends TaggedInt

trait Gen[A] {
  def gen2(gs: GenSize): Gen2[A]
  def map[B](g: A => B): Gen[B]
  def subst[AA >: A]: Gen[AA] = map(a => a: AA)
}

case class Gen2[A](f: SampleSize => EphemeralStream[A]) {
  def map[B](g: A => B) = Gen2[B](s => f(s) map g)
}

class RngGen[A](val f: GenSize => Rng[A]) extends Gen[A] {

  override def gen2(gs: GenSize): Gen2[A] =
    Gen2[A](ss => f(gs).fill(ss.value).map(EphemeralStream(_: _*)).run.unsafePerformIO())

  override def map[B](g: A => B): RngGen[B] = new RngGen[B](s => f(s) map g)
  override def subst[B >: A]    : RngGen[B] = map(a => a: B)

  def mapr[B](g: Rng[A] => Rng[B])    = new RngGen[B](g compose f)
  def flatMap[B](g: A => RngGen[B])   = new RngGen[B](s => f(s).flatMap(a => g(a).f(s)))
  def flatMapS[B](g: A => RngGenS[B]) = new RngGenS[B](s => f(s).flatMap(a => g(a).f(s)))

  private def sizeOp[B](g: Rng[A] => Size => Rng[B]): RngGenS[B] =
    new RngGenS(s => g(f(s))(s.value))

  private def sizeOp[B, C](g: Rng[A] => Size => Rng[B], h: B => C): RngGenS[C] =
    new RngGenS(s => g(f(s))(s.value) map h)

  def fill(n: Int)   : RngGen[List[A]]              = mapr(_ fill n)
  def list           : RngGenS[List[A]]             = sizeOp(_.list)
  def list1          : RngGenS[NonEmptyList[A]]     = sizeOp(_.list1)
  def set            : RngGenS[Set[A]]              = sizeOp(_.list, (_: List[A]).toSet)
  def vector         : RngGenS[Vector[A]]           = sizeOp(_.vector)
  def stream[AA >: A]: RngGenS[EphemeralStream[AA]] = sizeOp(_.stream)
  def option         : RngGen[Option[A]]            = mapr(_.option)

  //  def ***[X](x: Rng[X]): Rng[(A, X)] =
  //  def either[X](x: Rng[X]): Rng[A \/ X] =
  //  def \/[X](x: Rng[X]): Rng[A \/ X] =
  //  def validation[X](x: Rng[X]): Rng[A \?/ X] =
  //  def \?/[X](x: Rng[X]): Rng[A \?/ X] =
  //  def +++[X](x: Rng[X]): Rng[A \/ X] =
  //  def eitherS[X](x: Rng[X]): Rng[Either[A, X]] =
}

class RngGenS[A](f: GenSize => Rng[A]) extends RngGen(f) {

//  override def mapr[B](g: Rng[A] => Rng[B])  = new RngGenS[B](g compose f)
//  override def map[B](g: A => B)             = new RngGenS[B](s => f(s) map g)
//  override def flatMap[B](g: A => RngGen[B]) = new RngGenS[B](s => f(s).flatMap(a => g(a).f(s)))

  def lim(size: Int) = {
    val t = GenSize(size)
    new RngGenS[A](s => f(if (s.value > size) t else s))
  }
}

object Gen {

  object Covariance {
    implicit def rnggenCovariance[A, B >: A](r: RngGen[A]) = r.subst[B]
  }

  def unsized[A](rng: Rng[A]) =
    new RngGen[A](_ => rng)

  def lift[A](f: Size => Rng[A]) =
    new RngGenS[A](s => f(Size(s.value)))

  def double         : RngGen[Double]  = Rng.double.gen
  def float          : RngGen[Float]   = Rng.float.gen
  def long           : RngGen[Long]    = Rng.long.gen
  def int            : RngGen[Int]     = Rng.int.gen
  def byte           : RngGen[Byte]    = Rng.byte.gen
  def short          : RngGen[Short]   = Rng.short.gen
  def unit           : RngGen[Unit]    = Rng.unit.gen
  def boolean        : RngGen[Boolean] = Rng.boolean.gen
  def positivedouble : RngGen[Double]  = Rng.positivedouble.gen
  def negativedouble : RngGen[Double]  = Rng.negativedouble.gen
  def positivefloat  : RngGen[Float]   = Rng.positivefloat.gen
  def negativefloat  : RngGen[Float]   = Rng.negativefloat.gen
  def positivelong   : RngGen[Long]    = Rng.positivelong.gen
  def negativelong   : RngGen[Long]    = Rng.negativelong.gen
  def positiveint    : RngGen[Int]     = Rng.positiveint.gen
  def negativeint    : RngGen[Int]     = Rng.negativeint.gen
  def digit          : RngGen[Digit]   = Rng.digit.gen
  def numeric        : RngGen[Char]    = Rng.numeric.gen
  def char           : RngGen[Char]    = Rng.char.gen
  def upper          : RngGen[Char]    = Rng.upper.gen
  def lower          : RngGen[Char]    = Rng.lower.gen
  def alpha          : RngGen[Char]    = Rng.alpha.gen
  def alphanumeric   : RngGen[Char]    = Rng.alphanumeric.gen

  def digits              : RngGenS[List[Digit]]         = lift(Rng.digits)
  def digits1             : RngGenS[NonEmptyList[Digit]] = lift(Rng.digits1)
  def numerics            : RngGenS[List[Char]]          = lift(Rng.numerics)
  def numerics1           : RngGenS[NonEmptyList[Char]]  = lift(Rng.numerics1)
  def chars               : RngGenS[List[Char]]          = lift(Rng.chars)
  def chars1              : RngGenS[NonEmptyList[Char]]  = lift(Rng.chars1)
  def uppers              : RngGenS[List[Char]]          = lift(Rng.uppers)
  def uppers1             : RngGenS[NonEmptyList[Char]]  = lift(Rng.uppers1)
  def lowers              : RngGenS[List[Char]]          = lift(Rng.lowers)
  def lowers1             : RngGenS[NonEmptyList[Char]]  = lift(Rng.lowers1)
  def alphas              : RngGenS[List[Char]]          = lift(Rng.alphas)
  def alphas1             : RngGenS[NonEmptyList[Char]]  = lift(Rng.alphas1)
  def alphanumerics       : RngGenS[List[Char]]          = lift(Rng.alphanumerics)
  def alphanumerics1      : RngGenS[NonEmptyList[Char]]  = lift(Rng.alphanumerics1)
  def string              : RngGenS[String]              = lift(Rng.string)
  def string1             : RngGenS[String]              = lift(Rng.string1)
  def upperstring         : RngGenS[String]              = lift(Rng.upperstring)
  def upperstring1        : RngGenS[String]              = lift(Rng.upperstring1)
  def lowerstring         : RngGenS[String]              = lift(Rng.lowerstring)
  def lowerstring1        : RngGenS[String]              = lift(Rng.lowerstring1)
  def alphastring         : RngGenS[String]              = lift(Rng.alphastring)
  def alphastring1        : RngGenS[String]              = lift(Rng.alphastring1)
  def numericstring       : RngGenS[String]              = lift(Rng.numericstring)
  def numericstring1      : RngGenS[String]              = lift(Rng.numericstring1)
  def alphanumericstring  : RngGenS[String]              = lift(Rng.alphanumericstring)
  def alphanumericstring1 : RngGenS[String]              = lift(Rng.alphanumericstring1)
  def identifier          : RngGenS[NonEmptyList[Char]]  = lift(Rng.identifier)
  def identifierstring    : RngGenS[String]              = lift(Rng.identifierstring)
  def propernoun          : RngGenS[NonEmptyList[Char]]  = lift(Rng.propernoun)
  def propernounstring    : RngGenS[String]              = lift(Rng.propernounstring)

  def insert[A]    (a: A)                  : RngGen[A]      = Rng.insert(a).gen
  def chooselong   (l: Long, h: Long)      : RngGen[Long]   = Rng.chooselong(l,h).gen
  def choosedouble (l: Double, h: Double)  : RngGen[Double] = Rng.choosedouble(l,h).gen
  def choosefloat  (l: Float, h: Float)    : RngGen[Float]  = Rng.choosefloat(l,h).gen
  def chooseint    (l: Int, h: Int)        : RngGen[Int]    = Rng.chooseint(l,h).gen
  def oneofL[A]    (x: NonEmptyList[A])    : RngGen[A]      = Rng.oneofL(x).gen
  def oneof[A]     (a: A, as: A*)          : RngGen[A]      = Rng.oneof(a, as: _*).gen
  def oneofV[A]    (x: OneAnd[Vector, A])  : RngGen[A]      = Rng.oneofV(x).gen

//  def pair               [A, B](a: Gen[A], b: Gen[B]): Gen[(A, B)] = Rng.pair.gen
//  def triple             [A, B, C](a: Gen[A], b: Gen[B], c: Gen[C]): Gen[(A, B, C)] = Rng.triple.gen
//  def sequence           [T[_], A](x: T[Gen[A]])(implicit T: Traverse[T]): Gen[T[A]] = Rng.sequence.gen
//  def sequencePair       [X, A](x: X, r: Gen[A]): Gen[(X, A)] = Rng.sequencePair.gen
//  def distribute         [F[_], B](a: Gen[F[B]])(implicit D: Distributive[F]): F[Gen[B]] = Rng.distribute.gen
//  def distributeR        [A, B](a: Gen[A => B]): A => Gen[B] = Rng.distributeR.gen
//  def distributeRK       [A, B](a: Gen[A => B]): Kleisli[Gen, A, B] = Rng.distributeRK.gen
//  def distributeK        [F[_]: Distributive, A, B](a: Gen[Kleisli[F, A, B]]): Kleisli[F, A, Gen[B]] = Rng.distributeK.gen
//  def frequencyL         [A](x: NonEmptyList[(Int, Gen[A])]): Gen[A] = Rng.frequencyL.gen
//  def pick               (n: Int, l: NonEmptyList[(Int, Gen[A])]): Gen[A] = Rng.pick.gen
//  def frequency          [A](x: (Int, Gen[A]), xs: (Int, Gen[A])*): Gen[A] = Rng.frequency.gen

  def oneofG[A](a: RngGen[A], as: RngGen[A]*): RngGen[A] =
    Rng.oneof(a, as: _*).gen.flatMap(r => r)

  def oneofGC[A, B >: A](a: RngGen[A], as: RngGen[A]*): RngGen[B] =
    oneofG(a.subst[B], as.map(_.subst[B]): _*)
}