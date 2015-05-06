package shipreq.base.util

import scala.annotation.elidable
import scala.collection.GenTraversableOnce
import scala.collection.generic.Subtractable
import scalaz.{Order, Equal, Foldable}
import scalaz.std.iterable._
import scalaz.std.map._
import scalaz.syntax.foldable._

object IMapBase {
  def equality[K: Order, V: Equal, M <: IMapBase[K, V, M]]: Equal[M] =
    Equal.equalBy(_.underlyingMap)
}

abstract class IMapBase[K: UnivEq, V, This_ <: IMapBase[K, V, This_]] private[util] (m: Map[K, V]) extends Subtractable[K, This_] {
  final type This = This_

  override final def hashCode = m.##
  override final def equals(o: Any) = o match {
    case n: IMapBase[_, _, _] => m equals n.underlyingMap
    case n: Map[_, _]         => m equals n
    case _                    => false
  }

  protected def stringPrefix: String

  override final def toString = {
    val s = m.toString
    if (s startsWith m.stringPrefix)
      stringPrefix + s.drop(m.stringPrefix.length)
    else
      s"$stringPrefix($s)"
  }


  protected def gkey(v: V): K

  protected def setmap(n: Map[K, V]): This

  @inline final def underlyingMap = m
  @inline final def keys          = m.keys
  @inline final def values        = m.values
  @inline final def keySet        = m.keySet
  @inline final def size          = m.size

  @inline final def mapValues[A](f: V => A): Map[K, A] = m.mapValues(f)

  override final def -(k: K) =
    setmap(m - k)

  @inline final def +(v: V) = add(v)

  final def add(v: V) =
    setmap(m.updated(gkey(v), v))

  final def addAll(vs: V*) =
    addAllF(vs)

  final def addAllF[F[_]: Foldable](vs: F[V]) =
    setmap(vs.foldLeft(m)((n, v) => n.updated(gkey(v), v)))

  final def ++(vs: GenTraversableOnce[V]) =
    setmap(vs.foldLeft(m)((n, v) => n.updated(gkey(v), v)))

  final def vstream[A](f: V => A): Stream[A] =
    values.toStream.map(f)

  final def vstreamf[A](f: V => Stream[A]): Stream[A] =
    values.toStream.flatMap(f)

  @elidable(elidable.ASSERTION)
  final def assertValidKeys(m: Map[K, V]): Unit =
    for ((k1,v) <- m)
      assert(gkey(v) == k1, s"Expected key for [$v] is [${gkey(v)}] but [$k1] was found.")

  final def replaceUnderlying(n: Map[K, V]): This = {
    assertValidKeys(n)
    setmap(n)
  }

  final def mapUnderlying(f: Map[K, V] => Map[K, V]): This =
    replaceUnderlying(f(m))

  final def filter (f: (K, V) => Boolean): This = mapUnderlying(_ filter f.tupled)
  final def filterK(f: K      => Boolean): This = mapUnderlying(_ filterKeys f)
  final def filterV(f: V      => Boolean): This = mapUnderlying(_.filter(kv => f(kv._2)))

  final def filterNot (f: (K, V) => Boolean): This = mapUnderlying(_ filterNot f.tupled)
  final def filterNotK(f: K      => Boolean): This = mapUnderlying(_.filterNot(kv => f(kv._1)))
  final def filterNotV(f: V      => Boolean): This = mapUnderlying(_.filterNot(kv => f(kv._2)))
}
