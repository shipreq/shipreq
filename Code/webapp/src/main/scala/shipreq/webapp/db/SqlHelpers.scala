package shipreq.webapp
package db

import java.sql.Timestamp
import org.joda.time.DateTime
import scala.slick.jdbc.{SetParameter, GetResult}
import scala.slick.session.PositionedParameters
import lib.Types._
import feature.UcFilter
import shipreq.base.db.SqlHelpers._

object SqlHelpers {

  @inline implicit def shortToFieldKeyType(ordinal: Short): FieldKeyType = FieldKeyType(ordinal)

  implicit def TimestampToDateTime(t: Timestamp): DateTime = new DateTime(t)
  implicit val GR_DateTime = GetResult(r => TimestampToDateTime(r.nextTimestamp))
  implicit val GR_DateTimeOption = GetResult(r => r.nextTimestampOption.map(TimestampToDateTime))

  implicit val GR_UseCaseNumber = GR_TaggedShort[UseCaseNumber]
  implicit val SP_UseCaseNumber = SP_TaggedShort[UseCaseNumber]

  implicit val GR_FieldKeyId = GR_TaggedLong[FieldKeyId]
  implicit val SP_FieldKeyId = SP_TaggedLong[FieldKeyId]
  implicit val GR_UseCaseRevId = GR_TaggedLong[UseCaseRevId]
  implicit val SP_UseCaseRevId = SP_TaggedLong[UseCaseRevId]
  implicit val SP_UseCaseRevIdA = SP_TaggedLongArray[UseCaseRevId]
  implicit val GR_UseCaseIdentId = GR_TaggedLong[UseCaseIdentId]
  implicit val SP_UseCaseIdentId = SP_TaggedLong[UseCaseIdentId]
  implicit val SP_UseCaseIdentIdA = SP_TaggedLongArray[UseCaseIdentId]
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
}