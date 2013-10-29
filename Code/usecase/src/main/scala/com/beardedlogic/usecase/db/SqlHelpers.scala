package com.beardedlogic.usecase
package db

import java.sql.Timestamp
import org.joda.time.DateTime
import scala.slick.jdbc.{SetParameter, GetResult}
import scala.slick.session.{PositionedParameters, PositionedResult}
import lib.Types._
import feature.UcFilter

object SqlHelpers {

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

  private def GR_TaggedString[T <: TypeTag[String]]: GetResult[String @@ T] = GetResult(_.nextString.tag[T])
  private def GR_TaggedStringOpt[T <: TypeTag[String]]: GetResult[Option[String @@ T]] = GetResult(_.nextStringOption.tagInner[T])
  private def SP_TaggedString[Tag <: TypeTag[String]]: SetParameter[String @@ Tag] = new SetParameter[String @@ Tag] {
    def apply(v: String @@ Tag, pp: PositionedParameters): Unit = pp.setString(v)
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

  private def GR_Json[T]: GetResult[Json[T]] = GetResult(_.nextString.tag[IsJsonFor[T]])
  private def SP_Json[T]: SetParameter[Json[T]] = new SetParameter[Json[T]] {
    def apply(v: Json[T], pp: PositionedParameters): Unit = pp.setString(v)
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
  implicit val GR_ShareId = GR_TaggedLong[ShareId]
  implicit val SP_ShareId = SP_TaggedLong[ShareId]

  implicit val GR_NormalisedText = GR_TaggedString[IsNormalised]
  implicit val GR_ISO8601 = GR_TaggedString[ISO8601]
  implicit val GR_ISO8601Opt = GR_TaggedStringOpt[ISO8601]
  implicit val GR_ShareUrlToken = GR_TaggedString[IsShareUrlToken]
  implicit val SP_ShareUrlToken = SP_TaggedString[IsShareUrlToken]

  implicit val GR_JsonForUcFilter = GR_Json[UcFilter]
  implicit val SP_JsonForUcFilter = SP_Json[UcFilter]

  implicit val GR_FieldKeyType = GetResult(r => FieldKeyType(r.nextShort))
  implicit val SP_FieldKeyType: SetParameter[FieldKeyType] = new SetParameter[FieldKeyType] {
    def apply(v: FieldKeyType, pp: PositionedParameters): Unit = pp.setShort(v.id)
  }

  private[this] val LeadingWhitespace = """[\r\n]+\s*""".r

  implicit class SqlStringExt(val s: String) extends AnyVal {
    def sql = LeadingWhitespace.replaceAllIn(s, " ").trim
  }
}