package shipreq

import java.lang.CharSequence
import scala.annotation.elidable.ASSERTION
import scala.collection.{ArrayOps, StringOps, immutable}
import scala.reflect.ClassTag

// ===================================================================================================================
object \/ {
  def fromTryCatchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a) catch { case scala.util.control.NonFatal(e) => Left(e) }

  def fromTryCatch[A](a: => A): Either[Throwable, A] =
    try Right(a) catch { case e: Throwable => Left(e) }

  @inline def right[A](a: A): Right[Nothing, A] = Right(a)
  @inline def left [A](a: A): Left[A, Nothing] = Left(a)
}

final class EitherOps[E, A](private val e: Either[E, A]) extends AnyVal {
  @inline def castLeft() = e.asInstanceOf[Left[E, Nothing]]
  @inline def castRight() = e.asInstanceOf[Right[Nothing, A]]
}
// ===================================================================================================================


abstract class PredefShared
  extends PredefScala
     with cats.syntax.EitherSyntax
    //  with japgolly.microlibs.disjunction.Exports
     with japgolly.univeq.UnivEqCats
     with japgolly.univeq.UnivEqExports {


// ===================================================================================================================
// TODO: Fix japgolly.microlibs.disjunction.Exports
  final type -\/[+A] = scala.util.Left[A, Nothing]
  final type \/-[+A] = scala.util.Right[Nothing, A]

  @scala.annotation.showAsInfix
  final type \/[+A, +B] = Either[A, B]

  @inline final val \/  = shipreq.\/
  @inline final val -\/ = Left
  @inline final val \/- = Right
  // object \/- {
  //   @inline def apply[A](a: A): \/-[A] = Right(a)
  //   def unapply[A, B](e: Right[B, A]): Option[A] = if (e.isRight) Some(e.value) else None
  //   Right.un
  // }
  // object -\/ {
  //   @inline def apply[A](a: A): -\/[A] = Left(a)
  //   def unapply[A, B](e: Left[A, B]): Option[A] = if (e.isLeft) Some(e.value) else None
  // }

  @inline final implicit def implicitIgnoreLeftTypeOnRight[A, B, R](r: Right[A, R]): Right[B, R] =
    r.asInstanceOf[Right[B, R]]

  @inline final implicit def implicitIgnoreRightTypeOnLeft[A, B, L](r: Left[L, A]): Left[L, B] =
    r.asInstanceOf[Left[L, B]]

  @inline final implicit def implicitDisjEitherOps[E, A](a: Either[E, A]): shipreq.EitherOps[E, A] =
    new shipreq.EitherOps(a)
// ===================================================================================================================



  final type elidable = scala.annotation.elidable
  final val  elidable = scala.annotation.elidable
  final type nowarn   = scala.annotation.nowarn
  final type tailrec  = scala.annotation.tailrec

  final type ArraySeq[+A] = scala.collection.immutable.ArraySeq[A]
  final val  ArraySeq     = scala.collection.immutable.ArraySeq

  final type NonEmptySet[A] = japgolly.microlibs.nonempty.NonEmptySet[A]
  final val  NonEmptySet    = japgolly.microlibs.nonempty.NonEmptySet

  final type NonEmptyVector[+A] = japgolly.microlibs.nonempty.NonEmptyVector[A]
  final val  NonEmptyVector     = japgolly.microlibs.nonempty.NonEmptyVector

  final type Multimap[K, L[_], V] = japgolly.microlibs.multimap.Multimap[K, L, V]
  final val  Multimap             = japgolly.microlibs.multimap.Multimap

  final type MultiValues[L[_]] = japgolly.microlibs.multimap.MultiValues[L]
  final val  MultiValues       = japgolly.microlibs.multimap.MultiValues

  @inline
  @scala.annotation.nowarn("cat=unused")
  final implicit def univEqMultimap[K, L[_], V](implicit ev: UnivEq[Map[K, L[V]]]): UnivEq[Multimap[K, L, V]] =
    UnivEq.force

  @inline
  final implicit def UnivEqObjExt(self: UnivEq.type): PredefShared.UnivEqObjExt =
    new PredefShared.UnivEqObjExt(UnivEq)

  @inline
  final implicit def predefExtMultimapSet[K, V](a: Multimap[K, Set, V]): PredefShared.ExtMultimapSet[K, V] =
    new PredefShared.ExtMultimapSet(a)

  @inline
  final implicit def predefExtAny[A](a: A): PredefShared.ExtAny[A] =
    new PredefShared.ExtAny(a)

  @inline
  final implicit def predefExtInt(a: Int): PredefShared.ExtInt =
    new PredefShared.ExtInt(a)

  @inline
  final implicit def predefExtLong(a: Long): PredefShared.ExtLong =
    new PredefShared.ExtLong(a)

  @inline
  final implicit def predefExtAnyRef[A <: AnyRef](a: A): PredefShared.ExtAnyRef[A] =
    new PredefShared.ExtAnyRef(a)

  implicit def predefExtString(a: String): AnyVal with PredefShared.ExtString

  def ArraySeq1[@specialized A: ClassTag](a: A): ArraySeq[A] = {
    val x = new Array[A](1)
    x(0) = a
    ArraySeq.unsafeWrapArray(x)
  }

  @inline def Vector1[A](a: A): Vector[A] =
    Vector.empty :+ a

  @inline def Set1[A](a: A): Set[A] =
    Set.empty + a
}

object PredefShared {
  import japgolly.microlibs.multimap._
  import japgolly.univeq._
  import java.lang.String
  import scala.collection.immutable.Set

  // Copied from Shapeless
  trait =:!=[A, B]
  def _unexpected : Nothing = sys.error("Unexpected invocation")
  implicit def _neq[A, B] : A =:!= B = null.asInstanceOf[A =:!= B] //new =:!=[A, B] {}
  implicit def _neqAmbig1[A] : A =:!= A = _unexpected
  implicit def _neqAmbig2[A] : A =:!= A = _unexpected

  class UnivEqObjExt(private val self: UnivEq.type) extends AnyVal {
    @inline def emptySetMultimap[K: UnivEq, V: UnivEq] =
      Multimap.empty[K, immutable.Set, V]

    @inline def emptyMultimap[K: UnivEq, L[_] : MultiValues, V](implicit ev: L[V] =:!= immutable.Set[V]) =
      Multimap.empty[K, L, V]
  }

  final class ExtMultimapSet[K, V](private val m: Multimap[K, Set, V]) extends AnyVal {
    def asSubsetOf(f: Multimap[K, Set, V]): Multimap[K, Set, V] = {
      val ff = f.m
      val m2 = m.iterator.flatMap { case (k, vs) =>
          ff.get(k).flatMap { fvs =>
            val vs2 = vs.intersect(fvs)
            Option.when(vs2.nonEmpty)((k, vs2))
          }
        }.toMap
      new Multimap(m2)
    }
  }

  final class ExtAny[A](private val a: A) extends AnyVal {
    @inline def ===(rhs: A)(implicit e: cats.Eq[A]): Boolean = e.eqv(a, rhs)
    @inline def =!=(rhs: A)(implicit e: cats.Eq[A]): Boolean = e.neqv(a, rhs)
  }

  final class ExtInt(private val a: Int) extends AnyVal {
    type A = Int
    @inline def |>[@specialized B](f: A => B)   : B = f(a)
    @inline def <|                (f: A => Unit): A = {f(a); a}
  }
  final class ExtLong(private val a: Long) extends AnyVal {
    type A = Long
    @inline def |>[@specialized B](f: A => B)   : B = f(a)
    @inline def <|                (f: A => Unit): A = {f(a); a}
  }
  final class ExtAnyRef[A <: AnyRef](private val a: A) extends AnyVal {
    @inline def |>[@specialized B](f: A => B)   : B = f(a)
    @inline def <|                (f: A => Unit): A = {f(a); a}
  }

  trait ExtString extends Any {
    def quote: String

    def quoteInner: String = {
      val q = quote
      q.substring(1, q.length - 1)
    }
  }
}

// =====================================================================================================================

// Scala's Predef LowPriorityImplicits without extending LowPriorityImplicits2
sealed abstract class LowPriorityImplicits {
  @inline implicit final def predefByteWrapper              (x: Byte)               = scala.Predef.byteWrapper     (x)
  @inline implicit final def predefShortWrapper             (x: Short)              = scala.Predef.shortWrapper    (x)
  @inline implicit final def predefIntWrapper               (x: Int)                = scala.Predef.intWrapper      (x)
  @inline implicit final def predefCharWrapper              (c: Char)               = scala.Predef.charWrapper     (c)
  @inline implicit final def predefLongWrapper              (x: Long)               = scala.Predef.longWrapper     (x)
  @inline implicit final def predefFloatWrapper             (x: Float)              = scala.Predef.floatWrapper    (x)
  @inline implicit final def predefDoubleWrapper            (x: Double)             = scala.Predef.doubleWrapper   (x)
  @inline implicit final def predefBooleanWrapper           (x: Boolean)            = scala.Predef.booleanWrapper  (x)
  @inline implicit final def predefGenericWrapArray[T]      (xs: Array[T])          = scala.Predef.genericWrapArray(xs)
  @inline implicit final def predefWrapRefArray[T <: AnyRef](xs: Array[T])          = scala.Predef.wrapRefArray    (xs)
  @inline implicit final def predefWrapIntArray             (xs: Array[Int])        = scala.Predef.wrapIntArray    (xs)
  @inline implicit final def predefWrapDoubleArray          (xs: Array[Double])     = scala.Predef.wrapDoubleArray (xs)
  @inline implicit final def predefWrapLongArray            (xs: Array[Long])       = scala.Predef.wrapLongArray   (xs)
  @inline implicit final def predefWrapFloatArray           (xs: Array[Float])      = scala.Predef.wrapFloatArray  (xs)
  @inline implicit final def predefWrapCharArray            (xs: Array[Char])       = scala.Predef.wrapCharArray   (xs)
  @inline implicit final def predefWrapByteArray            (xs: Array[Byte])       = scala.Predef.wrapByteArray   (xs)
  @inline implicit final def predefWrapShortArray           (xs: Array[Short])      = scala.Predef.wrapShortArray  (xs)
  @inline implicit final def predefWrapBooleanArray         (xs: Array[Boolean])    = scala.Predef.wrapBooleanArray(xs)
  @inline implicit final def predefWrapUnitArray            (xs: Array[Unit])       = scala.Predef.wrapUnitArray   (xs)
  @inline implicit final def predefWrapString               (s: java.lang.String)   = scala.Predef.wrapString      (s)
}

sealed abstract class PredefScala extends LowPriorityImplicits {

//  @inline final def classOf[T]: Class[T] = scala.Predef.classOf[T]
//
//  @inline final def valueOf[T](implicit vt: ValueOf[T]): T = vt.value

  final type String = java.lang.String

  final type Class[T] = java.lang.Class[T]

  // miscellaneous -----------------------------------------------------
  scala.`package`                         // to force scala package object to be seen.
  scala.collection.immutable.List         // to force Nil, :: to be seen.

  final type Map[K, +V] = immutable.Map[K, V]
  final type Set[A]     = immutable.Set[A]
  final val Map         = immutable.Map
  final val Set         = immutable.Set

  final val -> = Tuple2

  @inline final def identity[A](x: A): A = x // see `$conforms` for the implicit version

  @inline final def implicitly[T](implicit e: T): T = e

  @inline final def locally[T](x: T): T = x

  // assertions ---------------------------------------------------------

  @scala.annotation.elidable(ASSERTION)
  @inline
  final def assert(assertion: Boolean): Unit =
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")

  @scala.annotation.elidable(ASSERTION)
  @inline
  final def assert(assertion: Boolean, message: => Any): Unit =
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed: "+ message)

  @scala.annotation.elidable(ASSERTION)
  @inline
  final def assert(error: Option[String]): Unit =
    assert(error.isEmpty, error.get)

  @inline final def ??? : Nothing = throw new NotImplementedError

  // implicit classes -----------------------------------------------------

  @inline final implicit def predefArrowAssoc[A](a: A): PredefScala.ArrowAssoc[A] = new PredefScala.ArrowAssoc(a)

  implicit final class SeqCharSequence(sequenceOfChars: scala.collection.IndexedSeq[Char]) extends CharSequence {
    def length: Int                                     = sequenceOfChars.length
    def charAt(index: Int): Char                        = sequenceOfChars(index)
    def subSequence(start: Int, end: Int): CharSequence = new SeqCharSequence(sequenceOfChars.slice(start, end))
    override def toString                               = sequenceOfChars.mkString
  }

  implicit final class ArrayCharSequence(arrayOfChars: Array[Char]) extends CharSequence {
    def length: Int                                     = arrayOfChars.length
    def charAt(index: Int): Char                        = arrayOfChars(index)
    def subSequence(start: Int, end: Int): CharSequence = new runtime.ArrayCharSequence(arrayOfChars, start, end)
    override def toString                               = arrayOfChars.mkString
  }

  @inline final implicit def predefAugmentString(x: String): StringOps = new StringOps(x)

  // printing -----------------------------------------------------------

  def print  (x: Any)                : Unit = Console.print(x)
  def println()                      : Unit = Console.println()
  def println(x: Any)                : Unit = Console.println(x)
  def printf (text: String, xs: Any*): Unit = Console.print(text.format(xs: _*))

  // views --------------------------------------------------------------

  @inline final implicit def predefGenericArrayOps[T]      (xs: Array[T])      : ArrayOps[T]       = scala.Predef.genericArrayOps(xs)
  @inline final implicit def predefBooleanArrayOps         (xs: Array[Boolean]): ArrayOps[Boolean] = scala.Predef.booleanArrayOps(xs)
  @inline final implicit def predefByteArrayOps            (xs: Array[Byte])   : ArrayOps[Byte]    = scala.Predef.byteArrayOps(xs)
  @inline final implicit def predefCharArrayOps            (xs: Array[Char])   : ArrayOps[Char]    = scala.Predef.charArrayOps(xs)
  @inline final implicit def predefDoubleArrayOps          (xs: Array[Double]) : ArrayOps[Double]  = scala.Predef.doubleArrayOps(xs)
  @inline final implicit def predefFloatArrayOps           (xs: Array[Float])  : ArrayOps[Float]   = scala.Predef.floatArrayOps(xs)
  @inline final implicit def predefIntArrayOps             (xs: Array[Int])    : ArrayOps[Int]     = scala.Predef.intArrayOps(xs)
  @inline final implicit def predefLongArrayOps            (xs: Array[Long])   : ArrayOps[Long]    = scala.Predef.longArrayOps(xs)
  @inline final implicit def predefRefArrayOps[T <: AnyRef](xs: Array[T])      : ArrayOps[T]       = scala.Predef.refArrayOps(xs)
  @inline final implicit def predefShortArrayOps           (xs: Array[Short])  : ArrayOps[Short]   = scala.Predef.shortArrayOps(xs)
  @inline final implicit def predefUnitArrayOps            (xs: Array[Unit])   : ArrayOps[Unit]    = scala.Predef.unitArrayOps(xs)

  // "Autoboxing" and "Autounboxing" ---------------------------------------------------

  @inline final implicit def predefBoxByte   (x: Byte   ): java.lang.Byte      = x.asInstanceOf[java.lang.Byte]
  @inline final implicit def predefBoxShort  (x: Short  ): java.lang.Short     = x.asInstanceOf[java.lang.Short]
  @inline final implicit def predefBoxChar   (x: Char   ): java.lang.Character = x.asInstanceOf[java.lang.Character]
  @inline final implicit def predefBoxInt    (x: Int    ): java.lang.Integer   = x.asInstanceOf[java.lang.Integer]
  @inline final implicit def predefBoxLong   (x: Long   ): java.lang.Long      = x.asInstanceOf[java.lang.Long]
  @inline final implicit def predefBoxFloat  (x: Float  ): java.lang.Float     = x.asInstanceOf[java.lang.Float]
  @inline final implicit def predefBoxDouble (x: Double ): java.lang.Double    = x.asInstanceOf[java.lang.Double]
  @inline final implicit def predefBoxBoolean(x: Boolean): java.lang.Boolean   = x.asInstanceOf[java.lang.Boolean]

  @inline final implicit def predefUnboxByte     (x: java.lang.Byte     ): Byte    = x.asInstanceOf[Byte]
  @inline final implicit def predefUnboxShort    (x: java.lang.Short    ): Short   = x.asInstanceOf[Short]
  @inline final implicit def predefUnboxCharacter(x: java.lang.Character): Char    = x.asInstanceOf[Char]
  @inline final implicit def predefUnboxInteger  (x: java.lang.Integer  ): Int     = x.asInstanceOf[Int]
  @inline final implicit def predefUnboxLong     (x: java.lang.Long     ): Long    = x.asInstanceOf[Long]
  @inline final implicit def predefUnboxFloat    (x: java.lang.Float    ): Float   = x.asInstanceOf[Float]
  @inline final implicit def predefUnboxDouble   (x: java.lang.Double   ): Double  = x.asInstanceOf[Double]
  @inline final implicit def predefUnboxBoolean  (x: java.lang.Boolean  ): Boolean = x.asInstanceOf[Boolean]

  @inline final implicit def predefConforms[A]: A => A = <:<.refl
}

object PredefScala {
  final class ArrowAssoc[A](private val self: A) extends AnyVal {
    @inline def ->[B](y: B): (A, B) = (self, y)
  }
}
