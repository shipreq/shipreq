package com.beardedlogic.usecase
package lib.db

import java.lang.{Long => JLong}
import java.sql.Timestamp
import org.joda.time.DateTime
import scala.Long
import scala.slick.jdbc.{SetParameter, GetResult}
import scala.slick.session.{PositionedParameters, PositionedResult}
import lib.Types._

object DBHelpers {

  import model._

  @inline implicit def shortToFieldKeyType(ordinal: Short): FieldKeyType = FieldKeyType(ordinal)
  @inline implicit def int2short(i: Int): Short = i.toShort

  implicit def TimestampToDateTime(t: Timestamp): DateTime = new DateTime(t)
  implicit val GRDateTime = GetResult(r => TimestampToDateTime(r.nextTimestamp))
  implicit val GRDateTimeOption = GetResult(r => r.nextTimestampOption.map(TimestampToDateTime))

  implicit class PositionedResultExt(val r: PositionedResult) extends AnyVal {
    def nextId[T <: JLong @@ TypeTag[Long]](): T = r.nextObject.asInstanceOf[T]
    def nextId_?[T <: JLong @@ TypeTag[Long]](): Option[T] = r.nextObjectOption.asInstanceOf[Option[T]]
  }

  private def GRTaggedLong[T <: JLong @@ TypeTag[Long]]: GetResult[T] = GetResult(_.nextId[T])
  private def SPTaggedLong[T <: JLong @@ TypeTag[Long]]: SetParameter[T] = new SetParameter[T] {
    def apply(v: T, pp: PositionedParameters): Unit = pp.setLong(v)
  }
  private def GRTaggedLongOpt[T <: JLong @@ TypeTag[Long]]: GetResult[Option[T]] = GetResult(_.nextId_?[T])
  private def SPTaggedLongOpt[T <: JLong @@ TypeTag[Long]] = new SetParameter[Option[T]] {
    def apply(v: Option[T], pp: PositionedParameters): Unit = pp.setObjectOption(v, java.sql.Types.BIGINT)
  }
  implicit val GRFieldKeyId = GRTaggedLong[FieldKeyId]
  implicit val SPFieldKeyId = SPTaggedLong[FieldKeyId]
  implicit val GRUseCaseRevId = GRTaggedLong[UseCaseRevId]
  implicit val SPUseCaseRevId = SPTaggedLong[UseCaseRevId]
  implicit val GRUseCaseIdentId = GRTaggedLong[UseCaseIdentId]
  implicit val SPUseCaseIdentId = SPTaggedLong[UseCaseIdentId]
  implicit val GRTextRevId = GRTaggedLong[TextRevId]
  implicit val SPTextRevId = SPTaggedLong[TextRevId]
  implicit val GRTextRevIdOpt = GRTaggedLongOpt[TextRevId]
  implicit val SPTextRevIdOpt = SPTaggedLongOpt[TextRevId]
  implicit val GRTextIdentId = GRTaggedLong[TextIdentId]
  implicit val SPTextIdentId = SPTaggedLong[TextIdentId]

  implicit val GRTextWithNormalisedRefs = GetResult(_.nextString.hasNormalisedRefs)

  implicit val GRFieldKeyType = GetResult(r => FieldKeyType(r.nextShort))
  implicit object SetParameterFieldKeyType extends SetParameter[FieldKeyType] {
    def apply(v: FieldKeyType, pp: PositionedParameters): Unit = pp.setShort(v.id)
  }

  val LeadingWhitespace = """[\r\n]+\s*""".r

  implicit class SqlStringExt(val s: String) extends AnyVal {
    def sql = LeadingWhitespace.replaceAllIn(s, " ").trim
  }

}