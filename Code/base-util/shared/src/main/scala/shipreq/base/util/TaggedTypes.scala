package shipreq.base.util

import japgolly.univeq.UnivEq
import scalaz.Order
import scalaz.std.string.stringInstance
import scalaz.std.anyVal.{intInstance, longInstance, shortInstance}

object TaggedTypes {

  trait TaggedType { // extends PreventToString
    /** The Underlying value type. */
    type U
    def value: U
    override def hashCode = value.hashCode
  }

  trait PreventToString {
    def value: Any
    override final def toString: Nothing =
      throw ArticulateError(s"${getClass.getSimpleName}($value).toString CALLED.")
  }

  trait TaggedInt extends TaggedType {
    final override type U = Int
    // Important: keep this is a val. It significantly affects ApplyEventBM
    final override val hashCode = value.hashCode
  }

  trait TaggedString extends TaggedType {
    final override type U = String
  }

  trait TaggedLong extends TaggedType {
    final override type U = Long
    // Tagged longs are usually IDs and usually used as map keys. (hence val)
    // Long arithmetic is super slow in JS. Casting to int before calculating hashcode is faster (says @sjrd)
    final override val hashCode = value.toInt.hashCode
  }

  trait TaggedShort  extends TaggedType {
    final override type U = Short
    final def toInt = value.toInt
  }

  final class TaggedTypeTypeclass[T <: TaggedType {type U = A}, A](implicit O: Order[A], SO: scala.Ordering[A]) {
    def subst[F <: TaggedType {type U = A}] = this.asInstanceOf[TaggedTypeTypeclass[F, A]]

    object ScalaTC extends scala.Ordering[T] {
      override def compare(x: T, y: T) = SO.compare(x.value, y.value)
    }
    object ScalazTC extends Order[T] with UnivEq[T] {
      override def equal(a1: T, a2: T) = a1 == a2
      override def equalIsNatural      = true
      override def order(x: T, y: T)   = O.order(x.value, y.value)
    }
  }

  private[this] val taggedTC_string = new TaggedTypeTypeclass[TaggedString, String]
  private[this] val taggedTC_long   = new TaggedTypeTypeclass[TaggedLong,   Long]
  private[this] val taggedTC_int    = new TaggedTypeTypeclass[TaggedInt,    Int]
  private[this] val taggedTC_short  = new TaggedTypeTypeclass[TaggedShort,  Short]

  implicit def taggedScalaTC_string[T <: TaggedType {type U = String}] = taggedTC_string.subst[T].ScalaTC
  implicit def taggedScalaTC_long  [T <: TaggedType {type U = Long  }] = taggedTC_long  .subst[T].ScalaTC
  implicit def taggedScalaTC_int   [T <: TaggedType {type U = Int   }] = taggedTC_int   .subst[T].ScalaTC
  implicit def taggedScalaTC_short [T <: TaggedType {type U = Short }] = taggedTC_short .subst[T].ScalaTC

  implicit def taggedScalazTC_string[T <: TaggedType {type U = String}] = taggedTC_string.subst[T].ScalazTC
  implicit def taggedScalazTC_long  [T <: TaggedType {type U = Long  }] = taggedTC_long  .subst[T].ScalazTC
  implicit def taggedScalazTC_int   [T <: TaggedType {type U = Int   }] = taggedTC_int   .subst[T].ScalazTC
  implicit def taggedScalazTC_short [T <: TaggedType {type U = Short }] = taggedTC_short .subst[T].ScalazTC

  implicit def autoUnboxTaggedString[T <: TaggedType {type U = String}](t: T): String = t.value
  implicit def autoUnboxTaggedLong  [T <: TaggedType {type U = Long}]  (t: T): Long   = t.value
  implicit def autoUnboxTaggedInt   [T <: TaggedType {type U = Int}]   (t: T): Int    = t.value
  implicit def autoUnboxTaggedShort [T <: TaggedType {type U = Short}] (t: T): Short  = t.value
}
