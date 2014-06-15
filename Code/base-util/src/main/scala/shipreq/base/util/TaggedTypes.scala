package shipreq.base.util

import scala.runtime.AbstractFunction1

object TaggedTypes {

  trait TaggedType {
    /** The Underlying value type. */
    type U

    def value: U
  }

  /** Typeclass for tagging types. */
  trait TaggedTypeCtor[T <: TaggedType] extends AbstractFunction1[T#U, T] {
    def apply(u: T#U): T
  }

  trait TaggedLong extends TaggedType { final type U = Long }
  trait TaggedShort extends TaggedType { final type U = Short }
  trait TaggedString extends TaggedType { final type U = String }

//  implicit def autoUnboxTaggedTypes[T <: TaggedType](t: T): T#U = t.value
  implicit def autoUnboxTaggedLong[T <: TaggedType](t: T)(implicit ev: T#U =:= Long): Long = ev(t.value)
  implicit def autoUnboxTaggedShort[T <: TaggedType](t: T)(implicit ev: T#U =:= Short): Short = ev(t.value)
  implicit def autoUnboxTaggedString[T <: TaggedType](t: T)(implicit ev: T#U =:= String): String = ev(t.value)

  // -------------------------------------------------------------------------------------------------------------------

  final case class JsonStr[R](value: String) extends TaggedString

  implicit def JsonStrCtor[R]: TaggedTypeCtor[JsonStr[R]] = new TaggedTypeCtor[JsonStr[R]] {
    override def apply(u: String) = JsonStr[R](u)
  }
}
