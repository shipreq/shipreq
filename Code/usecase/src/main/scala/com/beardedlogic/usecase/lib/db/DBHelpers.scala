package com.beardedlogic.usecase
package lib.db

import scala.slick.jdbc.GetResult
import lib.EnumValue
import java.sql.Timestamp
import org.joda.time.DateTime

object DBHelpers {
  import model._

  @inline implicit def enumToShort(enum: EnumValue): Short = enum.ordinal
  @inline implicit def shortToDataType(ordinal: Short): DataType = DataType(ordinal)
  @inline implicit def shortToRelationType(ordinal: Short): RelationType = RelationType(ordinal)
  @inline implicit def shortToFieldKeyType(ordinal: Short): FieldKeyType = FieldKeyType(ordinal)

  implicit val GetResultDataType = GetResult(r => DataType(r.nextShort))
  implicit val GetResultFieldKeyType = GetResult(r => FieldKeyType(r.nextShort))
  implicit val GetResultRelationType = GetResult(r => RelationType(r.nextShort))

  implicit def TimestampToDateTime(t: Timestamp): DateTime = new DateTime(t)
  implicit val GetResultDateTime = GetResult(r => TimestampToDateTime(r.nextTimestamp))
  implicit val GetResultDateTimeOption = GetResult(r => r.nextTimestampOption.map(TimestampToDateTime))

  val LeadingWhitespace = """[\r\n]+\s*""".r

  implicit class SqlStringExt(val s: String) extends AnyVal {
    def sql = LeadingWhitespace.replaceAllIn(s, " ").trim
  }
}