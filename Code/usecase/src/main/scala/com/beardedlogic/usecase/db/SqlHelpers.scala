package com.beardedlogic.usecase
package db

import java.sql.Timestamp
import org.joda.time.DateTime
import scala.Long
import scala.slick.jdbc.{SetParameter, GetResult}
import scala.slick.session.{PositionedParameters, PositionedResult}
import lib.Types._

private[db] object SqlHelpers {

  @inline implicit def shortToFieldKeyType(ordinal: Short): FieldKeyType = FieldKeyType(ordinal)

  implicit def TimestampToDateTime(t: Timestamp): DateTime = new DateTime(t)
  implicit val GR_DateTime = GetResult(r => TimestampToDateTime(r.nextTimestamp))
  implicit val GR_DateTimeOption = GetResult(r => r.nextTimestampOption.map(TimestampToDateTime))

  implicit class PositionedResultExt(val r: PositionedResult) extends AnyVal {
    def nextId[T <: JLong @@ TypeTag[JLong]](): T = r.nextObject.asInstanceOf[T]
    def nextId_?[T <: JLong @@ TypeTag[JLong]](): Option[T] = r.nextObjectOption.asInstanceOf[Option[T]]
    def nextTShort[Tag <: TypeTag[JShort]](): JShort @@ Tag = r.nextShort.tag[Tag]
    def nextTShort_?[Tag <: TypeTag[JShort]](): Option[JShort @@ Tag] = r.nextShortOption.map(_.tag[Tag])
  }

  private def GR_TaggedLong[T <: JLong @@ TypeTag[JLong]]: GetResult[T] = GetResult(_.nextId[T])
  private def SP_TaggedLong[T <: JLong @@ TypeTag[JLong]]: SetParameter[T] = new SetParameter[T] {
    def apply(v: T, pp: PositionedParameters): Unit = pp.setLong(v)
  }
  private def GR_TaggedLongOpt[T <: JLong @@ TypeTag[JLong]]: GetResult[Option[T]] = GetResult(_.nextId_?[T])
  private def SP_TaggedLongOpt[T <: JLong @@ TypeTag[JLong]] = new SetParameter[Option[T]] {
    def apply(v: Option[T], pp: PositionedParameters): Unit = pp.setObjectOption(v, java.sql.Types.BIGINT)
  }

  private def GR_TaggedShort[Tag <: TypeTag[JShort]]: GetResult[JShort @@ Tag] = GetResult(_.nextTShort[Tag])
  private def SP_TaggedShort[Tag <: TypeTag[JShort]]: SetParameter[JShort @@ Tag] = new SetParameter[JShort @@ Tag] {
    def apply(v: JShort @@ Tag, pp: PositionedParameters): Unit = pp.setShort(v)
  }

  implicit val GR_UseCaseNumber = GR_TaggedShort[UseCaseNumber]
  implicit val SP_UseCaseNumber = SP_TaggedShort[UseCaseNumber]
  implicit val GR_FieldKeyId = GR_TaggedLong[FieldKeyId]
  implicit val SP_FieldKeyId = SP_TaggedLong[FieldKeyId]
  implicit val GR_UseCaseRevId = GR_TaggedLong[UseCaseRevId]
  implicit val SP_UseCaseRevId = SP_TaggedLong[UseCaseRevId]
  implicit val GR_UseCaseIdentId = GR_TaggedLong[UseCaseIdentId]
  implicit val SP_UseCaseIdentId = SP_TaggedLong[UseCaseIdentId]
  implicit val GR_TextRevId = GR_TaggedLong[TextRevId]
  implicit val SP_TextRevId = SP_TaggedLong[TextRevId]
  implicit val GR_TextRevIdOpt = GR_TaggedLongOpt[TextRevId]
  implicit val SP_TextRevIdOpt = SP_TaggedLongOpt[TextRevId]
  implicit val GR_TextIdentId = GR_TaggedLong[TextIdentId]
  implicit val SP_TextIdentId = SP_TaggedLong[TextIdentId]
  implicit val GR_UserId = GR_TaggedLong[UserId]
  implicit val SP_UserId = SP_TaggedLong[UserId]
  implicit val GR_ProjectId = GR_TaggedLong[ProjectId]
  implicit val SP_ProjectId = SP_TaggedLong[ProjectId]

  implicit val GR_TextWithNormalisedRefs = GetResult(_.nextString.hasNormalisedRefs)

  implicit val GR_FieldKeyType = GetResult(r => FieldKeyType(r.nextShort))
  implicit object SetParameterFieldKeyType extends SetParameter[FieldKeyType] {
    def apply(v: FieldKeyType, pp: PositionedParameters): Unit = pp.setShort(v.id)
  }

  val LeadingWhitespace = """[\r\n]+\s*""".r

  implicit class SqlStringExt(val s: String) extends AnyVal {
    def sql = LeadingWhitespace.replaceAllIn(s, " ").trim
  }
}