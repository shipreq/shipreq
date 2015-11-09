package shipreq.base.util

import scala.collection.GenTraversable
import scala.reflect.ClassTag

object ScalaExt extends Platform.ScalaExt {

  type EndoFn[A] = A => A

  @inline final def none[A]: Option[A] = None

  implicit class BaseUtilExtAny[A](private val a: A) extends AnyVal {
    @inline def |>          [B]   (f: A => B)           : B      = f(a)
    @inline def <|                (f: A => Unit)        : A      = {f(a); a}
    @inline def mapStrengthL[B]   (f: A => B)           : (B, A) = (f(a), a)
    @inline def mapStrengthR[B]   (f: A => B)           : (A, B) = (a, f(a))
    @inline def tmap2       [B, C](b: A => B, c: A => C): (B, C) = (b(a), c(a))

    @inline def ifelse[B](c: A => Boolean, t: A => B, f: A => B): B =
      if (c(a)) t(a) else f(a)

    @inline def apif[B >: A](c: A => Boolean, t: A => B): B =
      if (c(a)) t(a) else a

    @inline def apif[B >: A](c: Boolean)(t: A => B): B =
      if (c) t(a) else a

    @inline def some: Option[A] = Some(a)
  }

  implicit class GenTravExt[T[x] <: GenTraversable[x], A](private val t: T[A]) extends AnyVal {
    def ifNonEmpty[B](f: T[A] => B): Option[B] =
      if (t.isEmpty) None else Some(f(t))
  }

  implicit class StringBuilderExt(val sb: StringBuilder) extends AnyVal {
    @inline def kv(k: String, v: Any): Unit = {
      sb append k
      sb append " = "
      sb append v
    }
    @inline def kv(k: String, v: Any, cond: Boolean): Unit = if (cond) kv(k, v)

    def mkStringF(start: String, sep: String, end: String)(fs: (StringBuilder => Any)*): Unit = {
      sb append start
      var nextSep = false
      for (f <- fs) {
        if (nextSep) sb append sep
        val b = sb.length
        f(sb)
        nextSep = sb.length != b
      }
      if (!nextSep) // undo sep if last function was NOP
        sb.setLength(sb.length - sep.length)
      sb append end
    }
  }

  implicit class StringExt(private val s: String) extends AnyVal {
    def flatMapSB(f: (Char, StringBuilder) => Unit): String =
      Util.quickSB(sb => s.foreach(c => f(c, sb)))
  }

  implicit class FuckingSomeExt[A](private val s: Some[A]) extends AnyVal {
    @inline def castOption: Option[A] = s.asInstanceOf[Option[A]]
  }

  implicit class IteratorExt[A](private val as: Iterator[A]) extends AnyVal {
    def filterT[T <: A](implicit t: ClassTag[T]): Iterator[T] =
      as.flatMap(t.unapply(_).iterator)
  }

  implicit class IterableExt[A](private val as: Iterable[A]) extends AnyVal {
    def filterT[T <: A](implicit t: ClassTag[T]): Stream[T] = // TODO deprecate?
      as.toStream.flatMap(t.unapply(_).toStream)

    def filterTI[T <: A](implicit t: ClassTag[T]): Iterator[T] =
      as.iterator.filterT[T]
  }

  implicit class VectorExt[A](private val as: Vector[A]) extends AnyVal {
    def isIndexValid(i: Int): Boolean =
      i < as.length && i >= 0

    def get(index: Int): Option[A] =
      getFlatMap(index)(Some(_))

    def getFlatMap[B](index: Int)(f: A => Option[B]): Option[B] =
      if (isIndexValid(index))
        f(as(index))
      else
        None

    def updateIndexOrNull[B >: A](index: Int, f: A => B): Vector[B] =
      if (isIndexValid(index))
        as.updated(index, f(as(index)))
      else
        null

    def updateIndex[B >: A](index: Int, f: A => B): Option[Vector[B]] =
      Option(updateIndexOrNull(index, f))

    def tryUpdateIndexOrNull[B >: A](index: Int, f: A => Option[B]): Vector[B] =
      if (isIndexValid(index))
        f(as(index)) match {
          case None    => null
          case Some(b) => as.updated(index, b)
        }
      else
        null

    def tryUpdateIndex[B >: A](index: Int, f: A => Option[B]): Option[Vector[B]] =
      Option(tryUpdateIndexOrNull(index, f))

    def insert[B >: A](index: Int, b: B): Option[Vector[B]] =
      if (index == 0)
        Some(b +: as)
      else if (index < 0)
        None
      else (index - as.length) match {
        case 0          => Some(as :+ b)
        case n if n < 0 => Some(as.patch(index, b :: Nil, 0))
        case _          => None
      }

    def deleteOrNull(index: Int): Vector[A] =
      if (isIndexValid(index))
        as.patch(index, Nil, 1)
      else
        null

    def delete(index: Int): Option[Vector[A]] =
      Option(deleteOrNull(index))
  }

  implicit class VectorNExt[A >: Null](private val as: Vector[A]) extends AnyVal {
    def getOrNull(index: Int): A =
      if (as.isIndexValid(index))
        as(index)
      else
        null
  }

  implicit class StreamExt[A](private val s: Stream[A]) extends AnyVal {
    @inline def distinctSafe(implicit ev: UnivEq[A]): Stream[A] =
      s.distinct
  }
  implicit class StreamOExt[A](private val s: Stream[Option[A]]) extends AnyVal {
    def somes: Stream[A] =
      s.filter(_.isDefined).map(_.get)
  }

  implicit class FuckingMapExt[K, V](private val m: Map[K, V]) extends AnyVal {
    def mapValuesNow[X](f: V => X): Map[K, X] = {
      val b = Map.newBuilder[K, X]
      for (t <- m)
        b += t.map2(f)
      b.result()
    }
  }

  // Generated by bin/gen-function_ext

  @inline final implicit class Function1Ext[X,Y](private val x: X ⇒ Y) extends AnyVal {
    @inline def unify(y: X⇒Y)(u: (⇒Y,⇒Y)⇒Y): X⇒Y = (a)⇒u(x(a),y(a))
    @inline def composeA[A,B](y: (A,B)⇒X): (A,B)⇒Y = y andThenA x
    @inline def composeA[A,B,C](y: (A,B,C)⇒X): (A,B,C)⇒Y = y andThenA x
    @inline def composeA[A,B,C,D](y: (A,B,C,D)⇒X): (A,B,C,D)⇒Y = y andThenA x
    @inline def composeA[A,B,C,D,E](y: (A,B,C,D,E)⇒X): (A,B,C,D,E)⇒Y = y andThenA x
    @inline def composeA[A,B,C,D,E,F](y: (A,B,C,D,E,F)⇒X): (A,B,C,D,E,F)⇒Y = y andThenA x
    @inline def composeA[A,B,C,D,E,F,G](y: (A,B,C,D,E,F,G)⇒X): (A,B,C,D,E,F,G)⇒Y = y andThenA x
    @inline def composeA[A,B,C,D,E,F,G,H](y: (A,B,C,D,E,F,G,H)⇒X): (A,B,C,D,E,F,G,H)⇒Y = y andThenA x
    @inline def composeA[A,B,C,D,E,F,G,H,I](y: (A,B,C,D,E,F,G,H,I)⇒X): (A,B,C,D,E,F,G,H,I)⇒Y = y andThenA x
    def overTuple2: HomoTuple2Map[X,Y] = _ mapEach x
    def overTuple3: HomoTuple3Map[X,Y] = _ mapEach x
    def overTuple4: HomoTuple4Map[X,Y] = _ mapEach x
    def overTuple5: HomoTuple5Map[X,Y] = _ mapEach x
    def overTuple6: HomoTuple6Map[X,Y] = _ mapEach x
    def overTuple7: HomoTuple7Map[X,Y] = _ mapEach x
    def overTuple8: HomoTuple8Map[X,Y] = _ mapEach x
    def overTuple9: HomoTuple9Map[X,Y] = _ mapEach x
  }
  @inline final implicit class Function2Ext[A,B,R](private val x: (A,B) ⇒ R) extends AnyVal {
    @inline def andThenA[Z](y: R⇒Z): (A,B)⇒Z = (a,b)⇒y(x(a,b))
    @inline def unify(y: (A,B)⇒R)(u: (⇒R,⇒R)⇒R): (A,B)⇒R = (a,b)⇒u(x(a,b),y(a,b))
  }
  @inline final implicit class Function3Ext[A,B,C,R](private val x: (A,B,C) ⇒ R) extends AnyVal {
    @inline def andThenA[Z](y: R⇒Z): (A,B,C)⇒Z = (a,b,c)⇒y(x(a,b,c))
    @inline def unify(y: (A,B,C)⇒R)(u: (⇒R,⇒R)⇒R): (A,B,C)⇒R = (a,b,c)⇒u(x(a,b,c),y(a,b,c))
  }
  @inline final implicit class Function4Ext[A,B,C,D,R](private val x: (A,B,C,D) ⇒ R) extends AnyVal {
    @inline def andThenA[Z](y: R⇒Z): (A,B,C,D)⇒Z = (a,b,c,d)⇒y(x(a,b,c,d))
    @inline def unify(y: (A,B,C,D)⇒R)(u: (⇒R,⇒R)⇒R): (A,B,C,D)⇒R = (a,b,c,d)⇒u(x(a,b,c,d),y(a,b,c,d))
  }
  @inline final implicit class Function5Ext[A,B,C,D,E,R](private val x: (A,B,C,D,E) ⇒ R) extends AnyVal {
    @inline def andThenA[Z](y: R⇒Z): (A,B,C,D,E)⇒Z = (a,b,c,d,e)⇒y(x(a,b,c,d,e))
    @inline def unify(y: (A,B,C,D,E)⇒R)(u: (⇒R,⇒R)⇒R): (A,B,C,D,E)⇒R = (a,b,c,d,e)⇒u(x(a,b,c,d,e),y(a,b,c,d,e))
  }
  @inline final implicit class Function6Ext[A,B,C,D,E,F,R](private val x: (A,B,C,D,E,F) ⇒ R) extends AnyVal {
    @inline def andThenA[Z](y: R⇒Z): (A,B,C,D,E,F)⇒Z = (a,b,c,d,e,f)⇒y(x(a,b,c,d,e,f))
    @inline def unify(y: (A,B,C,D,E,F)⇒R)(u: (⇒R,⇒R)⇒R): (A,B,C,D,E,F)⇒R = (a,b,c,d,e,f)⇒u(x(a,b,c,d,e,f),y(a,b,c,d,e,f))
  }
  @inline final implicit class Function7Ext[A,B,C,D,E,F,G,R](private val x: (A,B,C,D,E,F,G) ⇒ R) extends AnyVal {
    @inline def andThenA[Z](y: R⇒Z): (A,B,C,D,E,F,G)⇒Z = (a,b,c,d,e,f,g)⇒y(x(a,b,c,d,e,f,g))
    @inline def unify(y: (A,B,C,D,E,F,G)⇒R)(u: (⇒R,⇒R)⇒R): (A,B,C,D,E,F,G)⇒R = (a,b,c,d,e,f,g)⇒u(x(a,b,c,d,e,f,g),y(a,b,c,d,e,f,g))
  }
  @inline final implicit class Function8Ext[A,B,C,D,E,F,G,H,R](private val x: (A,B,C,D,E,F,G,H) ⇒ R) extends AnyVal {
    @inline def andThenA[Z](y: R⇒Z): (A,B,C,D,E,F,G,H)⇒Z = (a,b,c,d,e,f,g,h)⇒y(x(a,b,c,d,e,f,g,h))
    @inline def unify(y: (A,B,C,D,E,F,G,H)⇒R)(u: (⇒R,⇒R)⇒R): (A,B,C,D,E,F,G,H)⇒R = (a,b,c,d,e,f,g,h)⇒u(x(a,b,c,d,e,f,g,h),y(a,b,c,d,e,f,g,h))
  }
  @inline final implicit class Function9Ext[A,B,C,D,E,F,G,H,I,R](private val x: (A,B,C,D,E,F,G,H,I) ⇒ R) extends AnyVal {
    @inline def andThenA[Z](y: R⇒Z): (A,B,C,D,E,F,G,H,I)⇒Z = (a,b,c,d,e,f,g,h,i)⇒y(x(a,b,c,d,e,f,g,h,i))
    @inline def unify(y: (A,B,C,D,E,F,G,H,I)⇒R)(u: (⇒R,⇒R)⇒R): (A,B,C,D,E,F,G,H,I)⇒R = (a,b,c,d,e,f,g,h,i)⇒u(x(a,b,c,d,e,f,g,h,i),y(a,b,c,d,e,f,g,h,i))
  }

  @inline final implicit class Function1BoolExt[A](private val x: (A) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A) ⇒ Boolean = !x(_)
    @inline def &&(y: (A)⇒Boolean): (A)⇒Boolean = (a) ⇒ x(a) && y(a)
    @inline def ||(y: (A)⇒Boolean): (A)⇒Boolean = (a) ⇒ x(a) || y(a)
  }
  @inline final implicit class Function2BoolExt[A,B](private val x: (A,B) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B) ⇒ Boolean = !x(_,_)
    @inline def &&(y: (A,B)⇒Boolean): (A,B)⇒Boolean = (a,b) ⇒ x(a,b) && y(a,b)
    @inline def ||(y: (A,B)⇒Boolean): (A,B)⇒Boolean = (a,b) ⇒ x(a,b) || y(a,b)
  }
  @inline final implicit class Function3BoolExt[A,B,C](private val x: (A,B,C) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C) ⇒ Boolean = !x(_,_,_)
    @inline def &&(y: (A,B,C)⇒Boolean): (A,B,C)⇒Boolean = (a,b,c) ⇒ x(a,b,c) && y(a,b,c)
    @inline def ||(y: (A,B,C)⇒Boolean): (A,B,C)⇒Boolean = (a,b,c) ⇒ x(a,b,c) || y(a,b,c)
  }
  @inline final implicit class Function4BoolExt[A,B,C,D](private val x: (A,B,C,D) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D) ⇒ Boolean = !x(_,_,_,_)
    @inline def &&(y: (A,B,C,D)⇒Boolean): (A,B,C,D)⇒Boolean = (a,b,c,d) ⇒ x(a,b,c,d) && y(a,b,c,d)
    @inline def ||(y: (A,B,C,D)⇒Boolean): (A,B,C,D)⇒Boolean = (a,b,c,d) ⇒ x(a,b,c,d) || y(a,b,c,d)
  }
  @inline final implicit class Function5BoolExt[A,B,C,D,E](private val x: (A,B,C,D,E) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E) ⇒ Boolean = !x(_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E)⇒Boolean): (A,B,C,D,E)⇒Boolean = (a,b,c,d,e) ⇒ x(a,b,c,d,e) && y(a,b,c,d,e)
    @inline def ||(y: (A,B,C,D,E)⇒Boolean): (A,B,C,D,E)⇒Boolean = (a,b,c,d,e) ⇒ x(a,b,c,d,e) || y(a,b,c,d,e)
  }
  @inline final implicit class Function6BoolExt[A,B,C,D,E,F](private val x: (A,B,C,D,E,F) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F) ⇒ Boolean = !x(_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F)⇒Boolean): (A,B,C,D,E,F)⇒Boolean = (a,b,c,d,e,f) ⇒ x(a,b,c,d,e,f) && y(a,b,c,d,e,f)
    @inline def ||(y: (A,B,C,D,E,F)⇒Boolean): (A,B,C,D,E,F)⇒Boolean = (a,b,c,d,e,f) ⇒ x(a,b,c,d,e,f) || y(a,b,c,d,e,f)
  }
  @inline final implicit class Function7BoolExt[A,B,C,D,E,F,G](private val x: (A,B,C,D,E,F,G) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G) ⇒ Boolean = !x(_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G)⇒Boolean): (A,B,C,D,E,F,G)⇒Boolean = (a,b,c,d,e,f,g) ⇒ x(a,b,c,d,e,f,g) && y(a,b,c,d,e,f,g)
    @inline def ||(y: (A,B,C,D,E,F,G)⇒Boolean): (A,B,C,D,E,F,G)⇒Boolean = (a,b,c,d,e,f,g) ⇒ x(a,b,c,d,e,f,g) || y(a,b,c,d,e,f,g)
  }
  @inline final implicit class Function8BoolExt[A,B,C,D,E,F,G,H](private val x: (A,B,C,D,E,F,G,H) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H) ⇒ Boolean = !x(_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H)⇒Boolean): (A,B,C,D,E,F,G,H)⇒Boolean = (a,b,c,d,e,f,g,h) ⇒ x(a,b,c,d,e,f,g,h) && y(a,b,c,d,e,f,g,h)
    @inline def ||(y: (A,B,C,D,E,F,G,H)⇒Boolean): (A,B,C,D,E,F,G,H)⇒Boolean = (a,b,c,d,e,f,g,h) ⇒ x(a,b,c,d,e,f,g,h) || y(a,b,c,d,e,f,g,h)
  }
  @inline final implicit class Function9BoolExt[A,B,C,D,E,F,G,H,I](private val x: (A,B,C,D,E,F,G,H,I) ⇒ Boolean) extends AnyVal {
    @inline def unary_! : (A,B,C,D,E,F,G,H,I) ⇒ Boolean = !x(_,_,_,_,_,_,_,_,_)
    @inline def &&(y: (A,B,C,D,E,F,G,H,I)⇒Boolean): (A,B,C,D,E,F,G,H,I)⇒Boolean = (a,b,c,d,e,f,g,h,i) ⇒ x(a,b,c,d,e,f,g,h,i) && y(a,b,c,d,e,f,g,h,i)
    @inline def ||(y: (A,B,C,D,E,F,G,H,I)⇒Boolean): (A,B,C,D,E,F,G,H,I)⇒Boolean = (a,b,c,d,e,f,g,h,i) ⇒ x(a,b,c,d,e,f,g,h,i) || y(a,b,c,d,e,f,g,h,i)
  }


  // Generated by bin/gen-tuple_ext

  @inline final implicit class Tuple2Ext[A, B](private val t: (A, B)) extends AnyVal {
    @inline def consume1[U](f: A => U): B = {f(t._1); t._2}
    @inline def consume2[U](f: B => U): A = {f(t._2); t._1}
    @inline def map1[X](f: A => X): (X, B) = (f(t._1), t._2)
    @inline def map2[X](f: B => X): (A, X) = (t._1, f(t._2))
    @inline def put1[X](x: X): (X, B) = (x, t._2)
    @inline def put2[X](x: X): (A, X) = (t._1, x)
  }
  @inline final implicit class Tuple3Ext[A, B, C](private val t: (A, B, C)) extends AnyVal {
    @inline def consume1[U](f: A => U): (B, C) = {f(t._1); (t._2, t._3)}
    @inline def consume2[U](f: B => U): (A, C) = {f(t._2); (t._1, t._3)}
    @inline def consume3[U](f: C => U): (A, B) = {f(t._3); (t._1, t._2)}
    @inline def map1[X](f: A => X): (X, B, C) = (f(t._1), t._2, t._3)
    @inline def map2[X](f: B => X): (A, X, C) = (t._1, f(t._2), t._3)
    @inline def map3[X](f: C => X): (A, B, X) = (t._1, t._2, f(t._3))
    @inline def put1[X](x: X): (X, B, C) = (x, t._2, t._3)
    @inline def put2[X](x: X): (A, X, C) = (t._1, x, t._3)
    @inline def put3[X](x: X): (A, B, X) = (t._1, t._2, x)
  }
  @inline final implicit class Tuple4Ext[A, B, C, D](private val t: (A, B, C, D)) extends AnyVal {
    @inline def consume1[U](f: A => U): (B, C, D) = {f(t._1); (t._2, t._3, t._4)}
    @inline def consume2[U](f: B => U): (A, C, D) = {f(t._2); (t._1, t._3, t._4)}
    @inline def consume3[U](f: C => U): (A, B, D) = {f(t._3); (t._1, t._2, t._4)}
    @inline def consume4[U](f: D => U): (A, B, C) = {f(t._4); (t._1, t._2, t._3)}
    @inline def map1[X](f: A => X): (X, B, C, D) = (f(t._1), t._2, t._3, t._4)
    @inline def map2[X](f: B => X): (A, X, C, D) = (t._1, f(t._2), t._3, t._4)
    @inline def map3[X](f: C => X): (A, B, X, D) = (t._1, t._2, f(t._3), t._4)
    @inline def map4[X](f: D => X): (A, B, C, X) = (t._1, t._2, t._3, f(t._4))
    @inline def put1[X](x: X): (X, B, C, D) = (x, t._2, t._3, t._4)
    @inline def put2[X](x: X): (A, X, C, D) = (t._1, x, t._3, t._4)
    @inline def put3[X](x: X): (A, B, X, D) = (t._1, t._2, x, t._4)
    @inline def put4[X](x: X): (A, B, C, X) = (t._1, t._2, t._3, x)
  }
  @inline final implicit class Tuple5Ext[A, B, C, D, E](private val t: (A, B, C, D, E)) extends AnyVal {
    @inline def map1[X](f: A => X): (X, B, C, D, E) = (f(t._1), t._2, t._3, t._4, t._5)
    @inline def map2[X](f: B => X): (A, X, C, D, E) = (t._1, f(t._2), t._3, t._4, t._5)
    @inline def map3[X](f: C => X): (A, B, X, D, E) = (t._1, t._2, f(t._3), t._4, t._5)
    @inline def map4[X](f: D => X): (A, B, C, X, E) = (t._1, t._2, t._3, f(t._4), t._5)
    @inline def map5[X](f: E => X): (A, B, C, D, X) = (t._1, t._2, t._3, t._4, f(t._5))
    @inline def put1[X](x: X): (X, B, C, D, E) = (x, t._2, t._3, t._4, t._5)
    @inline def put2[X](x: X): (A, X, C, D, E) = (t._1, x, t._3, t._4, t._5)
    @inline def put3[X](x: X): (A, B, X, D, E) = (t._1, t._2, x, t._4, t._5)
    @inline def put4[X](x: X): (A, B, C, X, E) = (t._1, t._2, t._3, x, t._5)
    @inline def put5[X](x: X): (A, B, C, D, X) = (t._1, t._2, t._3, t._4, x)
  }
  @inline final implicit class Tuple6Ext[A, B, C, D, E, F](private val t: (A, B, C, D, E, F)) extends AnyVal {
    @inline def map1[X](f: A => X): (X, B, C, D, E, F) = (f(t._1), t._2, t._3, t._4, t._5, t._6)
    @inline def map2[X](f: B => X): (A, X, C, D, E, F) = (t._1, f(t._2), t._3, t._4, t._5, t._6)
    @inline def map3[X](f: C => X): (A, B, X, D, E, F) = (t._1, t._2, f(t._3), t._4, t._5, t._6)
    @inline def map4[X](f: D => X): (A, B, C, X, E, F) = (t._1, t._2, t._3, f(t._4), t._5, t._6)
    @inline def map5[X](f: E => X): (A, B, C, D, X, F) = (t._1, t._2, t._3, t._4, f(t._5), t._6)
    @inline def map6[X](f: F => X): (A, B, C, D, E, X) = (t._1, t._2, t._3, t._4, t._5, f(t._6))
    @inline def put1[X](x: X): (X, B, C, D, E, F) = (x, t._2, t._3, t._4, t._5, t._6)
    @inline def put2[X](x: X): (A, X, C, D, E, F) = (t._1, x, t._3, t._4, t._5, t._6)
    @inline def put3[X](x: X): (A, B, X, D, E, F) = (t._1, t._2, x, t._4, t._5, t._6)
    @inline def put4[X](x: X): (A, B, C, X, E, F) = (t._1, t._2, t._3, x, t._5, t._6)
    @inline def put5[X](x: X): (A, B, C, D, X, F) = (t._1, t._2, t._3, t._4, x, t._6)
    @inline def put6[X](x: X): (A, B, C, D, E, X) = (t._1, t._2, t._3, t._4, t._5, x)
  }
  @inline final implicit class Tuple7Ext[A, B, C, D, E, F, G](private val t: (A, B, C, D, E, F, G)) extends AnyVal {
    @inline def map1[X](f: A => X): (X, B, C, D, E, F, G) = (f(t._1), t._2, t._3, t._4, t._5, t._6, t._7)
    @inline def map2[X](f: B => X): (A, X, C, D, E, F, G) = (t._1, f(t._2), t._3, t._4, t._5, t._6, t._7)
    @inline def map3[X](f: C => X): (A, B, X, D, E, F, G) = (t._1, t._2, f(t._3), t._4, t._5, t._6, t._7)
    @inline def map4[X](f: D => X): (A, B, C, X, E, F, G) = (t._1, t._2, t._3, f(t._4), t._5, t._6, t._7)
    @inline def map5[X](f: E => X): (A, B, C, D, X, F, G) = (t._1, t._2, t._3, t._4, f(t._5), t._6, t._7)
    @inline def map6[X](f: F => X): (A, B, C, D, E, X, G) = (t._1, t._2, t._3, t._4, t._5, f(t._6), t._7)
    @inline def map7[X](f: G => X): (A, B, C, D, E, F, X) = (t._1, t._2, t._3, t._4, t._5, t._6, f(t._7))
    @inline def put1[X](x: X): (X, B, C, D, E, F, G) = (x, t._2, t._3, t._4, t._5, t._6, t._7)
    @inline def put2[X](x: X): (A, X, C, D, E, F, G) = (t._1, x, t._3, t._4, t._5, t._6, t._7)
    @inline def put3[X](x: X): (A, B, X, D, E, F, G) = (t._1, t._2, x, t._4, t._5, t._6, t._7)
    @inline def put4[X](x: X): (A, B, C, X, E, F, G) = (t._1, t._2, t._3, x, t._5, t._6, t._7)
    @inline def put5[X](x: X): (A, B, C, D, X, F, G) = (t._1, t._2, t._3, t._4, x, t._6, t._7)
    @inline def put6[X](x: X): (A, B, C, D, E, X, G) = (t._1, t._2, t._3, t._4, t._5, x, t._7)
    @inline def put7[X](x: X): (A, B, C, D, E, F, X) = (t._1, t._2, t._3, t._4, t._5, t._6, x)
  }
  @inline final implicit class Tuple8Ext[A, B, C, D, E, F, G, H](private val t: (A, B, C, D, E, F, G, H)) extends AnyVal {
    @inline def map1[X](f: A => X): (X, B, C, D, E, F, G, H) = (f(t._1), t._2, t._3, t._4, t._5, t._6, t._7, t._8)
    @inline def map2[X](f: B => X): (A, X, C, D, E, F, G, H) = (t._1, f(t._2), t._3, t._4, t._5, t._6, t._7, t._8)
    @inline def map3[X](f: C => X): (A, B, X, D, E, F, G, H) = (t._1, t._2, f(t._3), t._4, t._5, t._6, t._7, t._8)
    @inline def map4[X](f: D => X): (A, B, C, X, E, F, G, H) = (t._1, t._2, t._3, f(t._4), t._5, t._6, t._7, t._8)
    @inline def map5[X](f: E => X): (A, B, C, D, X, F, G, H) = (t._1, t._2, t._3, t._4, f(t._5), t._6, t._7, t._8)
    @inline def map6[X](f: F => X): (A, B, C, D, E, X, G, H) = (t._1, t._2, t._3, t._4, t._5, f(t._6), t._7, t._8)
    @inline def map7[X](f: G => X): (A, B, C, D, E, F, X, H) = (t._1, t._2, t._3, t._4, t._5, t._6, f(t._7), t._8)
    @inline def map8[X](f: H => X): (A, B, C, D, E, F, G, X) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, f(t._8))
    @inline def put1[X](x: X): (X, B, C, D, E, F, G, H) = (x, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
    @inline def put2[X](x: X): (A, X, C, D, E, F, G, H) = (t._1, x, t._3, t._4, t._5, t._6, t._7, t._8)
    @inline def put3[X](x: X): (A, B, X, D, E, F, G, H) = (t._1, t._2, x, t._4, t._5, t._6, t._7, t._8)
    @inline def put4[X](x: X): (A, B, C, X, E, F, G, H) = (t._1, t._2, t._3, x, t._5, t._6, t._7, t._8)
    @inline def put5[X](x: X): (A, B, C, D, X, F, G, H) = (t._1, t._2, t._3, t._4, x, t._6, t._7, t._8)
    @inline def put6[X](x: X): (A, B, C, D, E, X, G, H) = (t._1, t._2, t._3, t._4, t._5, x, t._7, t._8)
    @inline def put7[X](x: X): (A, B, C, D, E, F, X, H) = (t._1, t._2, t._3, t._4, t._5, t._6, x, t._8)
    @inline def put8[X](x: X): (A, B, C, D, E, F, G, X) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, x)
  }
  @inline final implicit class Tuple9Ext[A, B, C, D, E, F, G, H, I](private val t: (A, B, C, D, E, F, G, H, I)) extends AnyVal {
    @inline def map1[X](f: A => X): (X, B, C, D, E, F, G, H, I) = (f(t._1), t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def map2[X](f: B => X): (A, X, C, D, E, F, G, H, I) = (t._1, f(t._2), t._3, t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def map3[X](f: C => X): (A, B, X, D, E, F, G, H, I) = (t._1, t._2, f(t._3), t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def map4[X](f: D => X): (A, B, C, X, E, F, G, H, I) = (t._1, t._2, t._3, f(t._4), t._5, t._6, t._7, t._8, t._9)
    @inline def map5[X](f: E => X): (A, B, C, D, X, F, G, H, I) = (t._1, t._2, t._3, t._4, f(t._5), t._6, t._7, t._8, t._9)
    @inline def map6[X](f: F => X): (A, B, C, D, E, X, G, H, I) = (t._1, t._2, t._3, t._4, t._5, f(t._6), t._7, t._8, t._9)
    @inline def map7[X](f: G => X): (A, B, C, D, E, F, X, H, I) = (t._1, t._2, t._3, t._4, t._5, t._6, f(t._7), t._8, t._9)
    @inline def map8[X](f: H => X): (A, B, C, D, E, F, G, X, I) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, f(t._8), t._9)
    @inline def map9[X](f: I => X): (A, B, C, D, E, F, G, H, X) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, f(t._9))
    @inline def put1[X](x: X): (X, B, C, D, E, F, G, H, I) = (x, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def put2[X](x: X): (A, X, C, D, E, F, G, H, I) = (t._1, x, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def put3[X](x: X): (A, B, X, D, E, F, G, H, I) = (t._1, t._2, x, t._4, t._5, t._6, t._7, t._8, t._9)
    @inline def put4[X](x: X): (A, B, C, X, E, F, G, H, I) = (t._1, t._2, t._3, x, t._5, t._6, t._7, t._8, t._9)
    @inline def put5[X](x: X): (A, B, C, D, X, F, G, H, I) = (t._1, t._2, t._3, t._4, x, t._6, t._7, t._8, t._9)
    @inline def put6[X](x: X): (A, B, C, D, E, X, G, H, I) = (t._1, t._2, t._3, t._4, t._5, x, t._7, t._8, t._9)
    @inline def put7[X](x: X): (A, B, C, D, E, F, X, H, I) = (t._1, t._2, t._3, t._4, t._5, t._6, x, t._8, t._9)
    @inline def put8[X](x: X): (A, B, C, D, E, F, G, X, I) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, x, t._9)
    @inline def put9[X](x: X): (A, B, C, D, E, F, G, H, X) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, x)
  }

  // Generated by bin/gen-tuple_ext-homo

  @inline final implicit class HomoTuple2Ext[A](private val t: (A, A)) extends AnyVal {
    @inline def mapEach[B](f: A ⇒ B): (B, B) = (f(t._1), f(t._2))
  }
  @inline final implicit class HomoTuple3Ext[A](private val t: (A, A, A)) extends AnyVal {
    @inline def mapEach[B](f: A ⇒ B): (B, B, B) = (f(t._1), f(t._2), f(t._3))
  }
  @inline final implicit class HomoTuple4Ext[A](private val t: (A, A, A, A)) extends AnyVal {
    @inline def mapEach[B](f: A ⇒ B): (B, B, B, B) = (f(t._1), f(t._2), f(t._3), f(t._4))
  }
  @inline final implicit class HomoTuple5Ext[A](private val t: (A, A, A, A, A)) extends AnyVal {
    @inline def mapEach[B](f: A ⇒ B): (B, B, B, B, B) = (f(t._1), f(t._2), f(t._3), f(t._4), f(t._5))
  }
  @inline final implicit class HomoTuple6Ext[A](private val t: (A, A, A, A, A, A)) extends AnyVal {
    @inline def mapEach[B](f: A ⇒ B): (B, B, B, B, B, B) = (f(t._1), f(t._2), f(t._3), f(t._4), f(t._5), f(t._6))
  }
  @inline final implicit class HomoTuple7Ext[A](private val t: (A, A, A, A, A, A, A)) extends AnyVal {
    @inline def mapEach[B](f: A ⇒ B): (B, B, B, B, B, B, B) = (f(t._1), f(t._2), f(t._3), f(t._4), f(t._5), f(t._6), f(t._7))
  }
  @inline final implicit class HomoTuple8Ext[A](private val t: (A, A, A, A, A, A, A, A)) extends AnyVal {
    @inline def mapEach[B](f: A ⇒ B): (B, B, B, B, B, B, B, B) = (f(t._1), f(t._2), f(t._3), f(t._4), f(t._5), f(t._6), f(t._7), f(t._8))
  }
  @inline final implicit class HomoTuple9Ext[A](private val t: (A, A, A, A, A, A, A, A, A)) extends AnyVal {
    @inline def mapEach[B](f: A ⇒ B): (B, B, B, B, B, B, B, B, B) = (f(t._1), f(t._2), f(t._3), f(t._4), f(t._5), f(t._6), f(t._7), f(t._8), f(t._9))
  }

  type HomoTuple2[A] = (A, A)
  type HomoTuple3[A] = (A, A, A)
  type HomoTuple4[A] = (A, A, A, A)
  type HomoTuple5[A] = (A, A, A, A, A)
  type HomoTuple6[A] = (A, A, A, A, A, A)
  type HomoTuple7[A] = (A, A, A, A, A, A, A)
  type HomoTuple8[A] = (A, A, A, A, A, A, A, A)
  type HomoTuple9[A] = (A, A, A, A, A, A, A, A, A)

  type HomoTuple2Map[A,B] = ((A, A)) ⇒ (B, B)
  type HomoTuple3Map[A,B] = ((A, A, A)) ⇒ (B, B, B)
  type HomoTuple4Map[A,B] = ((A, A, A, A)) ⇒ (B, B, B, B)
  type HomoTuple5Map[A,B] = ((A, A, A, A, A)) ⇒ (B, B, B, B, B)
  type HomoTuple6Map[A,B] = ((A, A, A, A, A, A)) ⇒ (B, B, B, B, B, B)
  type HomoTuple7Map[A,B] = ((A, A, A, A, A, A, A)) ⇒ (B, B, B, B, B, B, B)
  type HomoTuple8Map[A,B] = ((A, A, A, A, A, A, A, A)) ⇒ (B, B, B, B, B, B, B, B)
  type HomoTuple9Map[A,B] = ((A, A, A, A, A, A, A, A, A)) ⇒ (B, B, B, B, B, B, B, B, B)

}
