package shipreq.webapp.server.db

import scala.slick.jdbc.{GetResult, PositionedParameters, PositionedResult, SetParameter}
import shipreq.base.db.SqlHelpers.{DbCodec => DBC, _}
import shipreq.base.db.JodaTimeSqlHelpers._
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.webapp.server.data._
import shipreq.webapp.server.security.{HashedStr, PasswordAndSalt}

object SqlHelpers {

  implicit val dbCodecEmailAddr     = DBC.WithOption.caseClass[EmailAddr]
  implicit val dbCodecHashedStr     = DBC.WithOption.caseClass[HashedStr]
  implicit val dbCodecProjectId     = DBC.WithOption.caseClass[ProjectId]
  implicit val dbCodecUserId        = DBC.WithOption.caseClass[UserId]
  implicit val dbCodecUsername      = DBC.WithOption.caseClass[Username]

  implicit val GR_PasswordAndSalt      = GetResult(r => PasswordAndSalt.restore(r.<<, r.<<))
  implicit val GR_ResetPasswordInfo    = GetResult(r => ResetPasswordInfo(r.<<, r.<<))
  implicit val GR_UserDescriptor       = GetResult(r => UserDescriptor(r.<<, r.<<, r.<<, userRoles(r)))
  implicit val GR_UserRegistrationInfo = GetResult(r => UserRegistrationInfo(r.<<, r.<<, r.<<, r.<<))
//  implicit val GR_UserSupplementalInfo = GetResult(r => UserSupplementalInfo(r.<<, r.<<))

  implicit object SP_PasswordAndSalt extends SetParameter[PasswordAndSalt] {
    def apply(v: PasswordAndSalt, pp: PositionedParameters) {
      pp.setString(v.hashedPassword.value)
      pp.setString(v.salt)
    }
  }

//  implicit val GR_UserDetail = GetResult(r => UserDetail(r.<<, r.<<))
//  implicit object SP_UserDetail extends SetParameter[UserDetail] {
//    def apply(d: UserDetail, pp: PositionedParameters) {
//      pp setString d.name
//      pp setBoolean d.newsletter
//    }
//  }

  def userRoles(r: PositionedResult): Set[String] =
    r.nextStringOption() match {
      case None        => Set.empty
      case Some(roles) => roles.split(',').toSet
    }
}