package shipreq.base.util

import scalaz.{Order, Equal}
import scalaz.std.string.stringInstance
import scalaz.std.anyVal.{shortInstance, longInstance, intInstance}

object TaggedTypes {

  trait TaggedType { // extends PreventToString
    /** The Underlying value type. */
    type U
    def value: U
  }

  trait PreventToString {
    def value: Any
    override final def toString: Nothing = {
      val expl = s"${getClass.getSimpleName}($value).toString CALLED."
      println("=" * 120)
      println(expl)
      println(Error.stackTraceStr(new Throwable()).lines.filter(_ contains "shipreq").mkString("\n"))
      println("-" * 80)
      throw new RuntimeException(expl)
    }
  }

  /** Typeclass for tagging types. */
  trait TaggedTypeCtor[T <: TaggedType] { // Don't add AbstractFunction1[T#U, T] as it causes autoboxing
    def apply(u: T#U): T
  }
  object TaggedTypeCtor {
    def apply[T <: TaggedType](f: T#U => T): TaggedTypeCtor[T] = new TaggedTypeCtor[T] {
      override def apply(u: T#U) = f(u)
    }
  }

  trait TaggedLong extends TaggedType { final type U = Long }
  trait TaggedInt extends TaggedType { final type U = Int }
  trait TaggedString extends TaggedType { final type U = String }
  trait TaggedShort extends TaggedType {
    final type U = Short
    final def toInt = value.toInt
  }

  final class TaggedTypeTypeclass[T <: TaggedType {type U = A}, A](implicit O: Order[A], SO: scala.Ordering[A]) {
    def subst[F <: TaggedType {type U = A}] = this.asInstanceOf[TaggedTypeTypeclass[F, A]]

    object ScalaTC extends scala.Ordering[T] {
      override def compare(x: T, y: T) = SO.compare(x.value, y.value)
    }
    object ScalazTC extends Order[T] {
      override def equal(a1: T, a2: T) = O.equal(a1.value, a2.value)
      override def equalIsNatural      = O.equalIsNatural
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

//  implicit def autoUnboxTaggedTypes[T <: TaggedType](t: T): T#U = t.value
//  implicit def autoUnboxTaggedLong[T <: TaggedType](t: T)(implicit ev: T#U =:= Long): Long = ev(t.value)
//  implicit def autoUnboxTaggedShort[T <: TaggedType](t: T)(implicit ev: T#U =:= Short): Short = ev(t.value)
//  implicit def autoUnboxTaggedString[T <: TaggedType](t: T)(implicit ev: T#U =:= String): String = ev(t.value)
//  implicit def autoUnboxTaggedTypes[UU, T <: TaggedType {type U = UU}](t: T): UU = t.value
  implicit def autoUnboxTaggedLong  [T <: TaggedType {type U = Long}]  (t: T): Long   = t.value
  implicit def autoUnboxTaggedInt   [T <: TaggedType {type U = Int}]   (t: T): Int    = t.value
  implicit def autoUnboxTaggedShort [T <: TaggedType {type U = Short}] (t: T): Short  = t.value
  implicit def autoUnboxTaggedString[T <: TaggedType {type U = String}](t: T): String = t.value

  // -------------------------------------------------------------------------------------------------------------------

  final case class JsonStr[R](value: String) extends TaggedString
  implicit def JsonStrCtor[R] = TaggedTypeCtor[JsonStr[R]](JsonStr[R])
}
