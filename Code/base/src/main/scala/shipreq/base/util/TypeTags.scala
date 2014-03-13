package shipreq.base.util

import java.lang.{Long => JJLong, Short => JJShort}

object TypeTags extends TypeTagsObjectMixin {

  trait TypeTag[-O <: AnyRef]

  // Taggable version of Unit
  sealed trait Unit2 extends AnyRef
  final object Unit2 extends Unit2 {
    override def toString = "()"
  }

  implicit def taggedStringOrdering[T <: TypeTag[String]] =
    implicitly[Ordering[String]].asInstanceOf[Ordering[String @@ T]]

  implicit class TTAnyRefTypeTagExt[A <: AnyRef](val __a: A) extends AnyVal {
    def tag[T <: TypeTag[A]] = __a.asInstanceOf[A @@ T]
  }

//  implicit class TTTaggedTypeExt[A <: AnyRef, T <: TypeTag[_]](val x: A @@ T) extends AnyVal {
//    def untag: A = x //.asInstanceOf[A]
//    def contra[AA >: A]: AA = untag
//  }

  implicit class TTStringTypeTagExt2[F[_]](val s: F[String]) extends AnyVal {
    def tagInner[T <: TypeTag[String]] = s.asInstanceOf[F[String @@ T]]
  }

  implicit class TTStringTypeTagExt3[G[_], F[G]](val s: F[G[String]]) extends AnyVal {
    def tagInner[T <: TypeTag[String]] = s.asInstanceOf[F[G[String @@ T]]]
  }

  implicit class TTSLongTypeTagExt(val x: scala.Long) extends AnyVal {
    def tag[T <: TypeTag[JLong]] = JJLong.valueOf(x).tag[T]
  }

  implicit class TTSLongOptionTypeTagExt(val x: Option[scala.Long]) extends AnyVal {
    def tag[T <: TypeTag[JLong]] = x.map(_.tag[T])
  }

  implicit class TTSShortTypeTagExt(val x: scala.Short) extends AnyVal {
    def tag[T <: TypeTag[JShort]] = JJShort.valueOf(x).tag[T]
  }

  implicit class TTSShortOptionTypeTagExt(val x: Option[scala.Short]) extends AnyVal {
    def tag[T <: TypeTag[JShort]] = x.map(_.tag[T])
  }

  sealed trait IsJsonFor[T] extends TypeTag[String]
}

/**
 * This stuff gets mixed into the TypeTags object.
 *
 * Anything defined in the object must be omitted from this trait.
 */
private[util] trait TypeTagsObjectMixin {
  import TypeTags._

  final type JLong = JJLong
  final type JShort = JJShort

  final type @@[+O <: AnyRef, T <: TypeTag[O]] = O with T
  final type Json[T] = String @@ IsJsonFor[T]
}

/**
 * A trait version of the TypeTags object.
 *
 * Includes aliases to stuff defined in the object.
 * There should be no difference between extending this and importing from the object directly.
 */
trait TypeTags extends TypeTagsObjectMixin {
  final type TypeTag[-O <: AnyRef] = TypeTags.TypeTag[O]
  final type Unit2 = TypeTags.Unit2
  final val Unit2 = TypeTags.Unit2
  final implicit def taggedStringOrdering[T <: TypeTag[String]] = TypeTags.taggedStringOrdering[T]
  final implicit def TTAnyRefTypeTagExt[A <: AnyRef](a: A) = TypeTags.TTAnyRefTypeTagExt(a)
  final implicit def TTStringTypeTagExt2[F[_]](s: F[String]) = TypeTags.TTStringTypeTagExt2(s)
  final implicit def TTStringTypeTagExt3[G[_], F[G]](s: F[G[String]]) = TypeTags.TTStringTypeTagExt3(s)
  final implicit def TTSLongTypeTagExt(x: scala.Long) = TypeTags.TTSLongTypeTagExt(x)
  final implicit def TTSLongOptionTypeTagExt(x: Option[scala.Long]) = TypeTags.TTSLongOptionTypeTagExt(x)
  final implicit def TTSShortTypeTagExt(x: scala.Short) = TypeTags.TTSShortTypeTagExt(x)
  final implicit def TTSShortOptionTypeTagExt(x: Option[scala.Short]) = TypeTags.TTSShortOptionTypeTagExt(x)
  final type IsJsonFor[T] = TypeTags.IsJsonFor[T]
}
