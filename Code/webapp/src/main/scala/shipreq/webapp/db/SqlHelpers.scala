package shipreq.webapp
package db

import scala.slick.jdbc.{GetResult, SetParameter, PositionedResult, PositionedParameters}
import shipreq.base.db.SqlHelpers._
import shipreq.base.db.JodaTimeSqlHelpers._
import shipreq.taskman.api.{EmailAddr, UserId}
import lib.Types._
import feature.UcFilter
import security.PasswordAndSalt

object SqlHelpers {

  @inline implicit def shortToFieldKeyType(ordinal: Short): FieldKeyType = FieldKeyType(ordinal)

  implicit val (ea1, ea2, ea3, ea4) = sqlAccessors[EmailAddr]
  implicit val (fk1, fk2, fk3, fk4) = sqlAccessors[FieldKeyId]
  implicit val (hs1, hs2, hs3, hs4) = sqlAccessors[HashedStr]
  implicit val (i81, i82, i83, i84) = sqlAccessors[ISO8601]
  implicit val (nt1, nt2, nt3, nt4) = sqlAccessors[NormalisedText]
  implicit val (pi1, pi2, pi3, pi4) = sqlAccessors[ProjectId]
  implicit val (si1, si2, si3, si4) = sqlAccessors[ShareId]
  implicit val (su1, su2, su3, su4) = sqlAccessors[ShareUrlToken]
  implicit val (sl1, sl2, sl3, sl4) = sqlAccessors[StepLabel]
  implicit val (ti1, ti2, ti3, ti4) = sqlAccessors[TextIdentId]
  implicit val (tr1, tr2, tr3, tr4) = sqlAccessors[TextRevId]
  implicit val (ui1, ui2, ui3, ui4) = sqlAccessors[UserId]
  implicit val (uc1, uc2, uc3, uc4) = sqlAccessors[UseCaseIdentId]
  implicit val (un1, un2, un3, un4) = sqlAccessors[UseCaseNumber]
  implicit val (ur1, ur2, ur3, ur4) = sqlAccessors[UseCaseRevId]
  implicit val (um1, um2, um3, um4) = sqlAccessors[Username]
  implicit val (uf1, uf2, uf3, uf4) = sqlAccessorsJson[UcFilter]
  implicit val ucl = SP_TaggedLongL[UseCaseIdentId]
  implicit val url = SP_TaggedLongL[UseCaseRevId]

  implicit val GR_FieldKeyType = GetResult(r => FieldKeyType(r.<<))
  implicit val SP_FieldKeyType: SetParameter[FieldKeyType] = new SetParameter[FieldKeyType] {
    def apply(v: FieldKeyType, pp: PositionedParameters): Unit = pp.setShort(v.id)
  }

  implicit val GR_FieldKey = GetResult(r => FieldKeyRec(r.<<, r.<<, r.<<))
  implicit val GR_PasswordAndSalt = GetResult(r => PasswordAndSalt.restore(r.<<, r.<<))
  implicit val GR_Project = GetResult(r => Project(r.<<, r.<<, r.<<))
  implicit val GR_ProjectSummary = GetResult(r => ProjectSummary(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))
  implicit val GR_ResetPasswordInfo = GetResult(r => ResetPasswordInfo(r.<<, r.<<))
  implicit val GR_Share = GetResult(r => Share(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))
  implicit val GR_ShareSummary = GetResult(r => ShareSummary(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))
  implicit val GR_TextRev = GetResult(r => TextRev(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UcFieldText = GetResult(r => UcFieldText(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UcFieldTextWithFK = GetResult(r => UcFieldTextWithFK(r.<<, r.<<))
  implicit val GR_UseCaseIdent = GetResult(r => UseCaseIdent(r.<<, r.<<, r.<<))
  implicit val GR_UseCaseRev = GetResult(r => UseCaseRev(r.<<, r.<<, r.<<, UseCaseHeader(r.<<), r.<<))
  implicit val GR_UseCaseSummary = GetResult(r => UseCaseSummary(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UserDescriptor = GetResult(r => UserDescriptor(r.<<, r.<<, r.<<, userRoles(r)))
  implicit val GR_UserRegistrationInfo = GetResult(r => UserRegistrationInfo(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UserSupplementalInfo = GetResult(r => UserSupplementalInfo(r.<<, r.<<))

  implicit object SP_PasswordAndSalt extends SetParameter[PasswordAndSalt] {
    def apply(v: PasswordAndSalt, pp: PositionedParameters) {
      pp.setString(v.hashedPassword)
      pp.setString(v.salt)
    }
  }

  implicit val GR_UserDetail = GetResult(r => UserDetail(r.<<, r.<<))
  implicit object SP_UserDetail extends SetParameter[UserDetail] {
    def apply(d: UserDetail, pp: PositionedParameters) {
      pp setString d.name
      pp setBoolean d.newsletter
    }
  }

  def userRoles(r: PositionedResult): Set[String] =
    r.nextStringOption() match {
      case None        => Set.empty
      case Some(roles) => roles.split(',').toSet
    }
}