package shipreq.base.util

import scalaz.Equal
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

  final class TaggedTypeScalaz[T <: TaggedType {type U = A}, A](implicit E: Equal[A]) extends Equal[T] {
    override def equal(a1: T, a2: T) = E.equal(a1.value, a2.value)
    override def equalIsNatural = E.equalIsNatural
    def subst[F <: TaggedType {type U = A}] = this.asInstanceOf[TaggedTypeScalaz[F, A]]
  }
  private val _taggedStringInstance = new TaggedTypeScalaz[TaggedString, String]
  private val _taggedLongInstance   = new TaggedTypeScalaz[TaggedLong, Long]
  private val _taggedIntInstance    = new TaggedTypeScalaz[TaggedInt, Int]
  private val _taggedShortInstance  = new TaggedTypeScalaz[TaggedShort, Short]

  implicit def taggedStringInstance[T <: TaggedType {type U = String}] = _taggedStringInstance.subst[T]
  implicit def taggedLongInstance  [T <: TaggedType {type U = Long}]   = _taggedLongInstance.subst[T]
  implicit def taggedIntInstance  [T <: TaggedType {type U = Int}]     = _taggedIntInstance.subst[T]
  implicit def taggedShortInstance [T <: TaggedType {type U = Short}]  = _taggedShortInstance.subst[T]

//  implicit def autoUnboxTaggedTypes[T <: TaggedType](t: T): T#U = t.value
//  implicit def autoUnboxTaggedLong[T <: TaggedType](t: T)(implicit ev: T#U =:= Long): Long = ev(t.value)
//  implicit def autoUnboxTaggedShort[T <: TaggedType](t: T)(implicit ev: T#U =:= Short): Short = ev(t.value)
//  implicit def autoUnboxTaggedString[T <: TaggedType](t: T)(implicit ev: T#U =:= String): String = ev(t.value)
//  implicit def autoUnboxTaggedTypes[UU, T <: TaggedType {type U = UU}](t: T): UU = t.value
  implicit def autoUnboxTaggedLong[T <: TaggedType {type U = Long}](t: T): Long = t.value
  implicit def autoUnboxTaggedInt[T <: TaggedType {type U = Int}](t: T): Int = t.value
  implicit def autoUnboxTaggedShort[T <: TaggedType {type U = Short}](t: T): Short = t.value
  implicit def autoUnboxTaggedString[T <: TaggedType {type U = String}](t: T): String = t.value

  // -------------------------------------------------------------------------------------------------------------------

  final case class JsonStr[R](value: String) extends TaggedString
  implicit def JsonStrCtor[R] = TaggedTypeCtor[JsonStr[R]](JsonStr[R])

}
