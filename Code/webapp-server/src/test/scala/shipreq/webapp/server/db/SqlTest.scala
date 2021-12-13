package shipreq.webapp.server.db

import shipreq.base.test.db.TestDb
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.member.test.project.UnsafeTypes._
import shipreq.webapp.server.test.PrepareEnv
import utest._

object SqlTest extends TestSuite {
  import DbInterpreter._

  private lazy val db = new DbInterpreter()(PrepareEnv.global().config.server.security)

  override def tests = Tests {

    "base" - {
      "logGlobalEventSql" - TestDb.check(db.logGlobalEventSql)
    }

    "security" - {
      val db = ForSecurity
      "getUserAndPasswordByEmailSql"    - TestDb.check(db.getUserAndPasswordByEmailSql)
      "getUserAndPasswordByUsernameSql" - TestDb.check(db.getUserAndPasswordByUsernameSql)
      "logLoginSuccessSql"              - TestDb.check(db.logLoginSuccessSql)
      "getProjectOwnerSql"              - TestDb.check(db.getProjectOwnerSql)
    }

    "verificationTokenReadOnly" - {
      val db = VerificationTokenReadOnly
      "getUserRegistrationTokenIssueDateSql" - TestDb.check(db.getUserRegistrationTokenIssueDateSql)
      "getResetPasswordTokenIssueDateSql"    - TestDb.check(db.getResetPasswordTokenIssueDateSql)
    }

    "publicSpa" - {
      "getUserIdByEmailSql"                  - TestDb.check(db.getUserIdByEmailSql)
      "getUserIdByUsernameSql"               - TestDb.check(db.getUserIdByUsernameSql)
      "getUserRegistrationSql"               - TestDb.check(db.getUserRegistrationSql)
      "createUserPlaceholderSql"             - TestDb.check(db.createUserPlaceholderSql)
      "updateUserRegistrationTokenSql"       - TestDb.check(db.updateUserRegistrationTokenSql)
      "sqlRegisterUser"                      - TestDb.check(db.sqlRegisterUser)
      "sqlInsertUsrd"                        - TestDb.check(db.sqlInsertUsrd)
      "getPasswordResetStateByEmailSql"      - TestDb.check(db.getPasswordResetStateByEmailSql)
      "getPasswordResetStateByUsernameSql"   - TestDb.check(db.getPasswordResetStateByUsernameSql)
      "createResetPasswordTokenSql"          - TestDb.check(db.createResetPasswordTokenSql)
      "updateResetPasswordTokenOnReissueSql" - TestDb.check(db.updateResetPasswordTokenOnReissueSql)
      "updateUserPasswordSql"                - TestDb.check(db.updateUserPasswordSql)
    }

    "getProjectMetaData" - {
      "projectMetaDataQuery" - TestDb.check(db.getProjectMetaDataQuery)
    }

    "getProjectEvents" - {
      val pid = ProjectId(2)
      "all"   - TestDb.check(GetProjectEventLogic.all(pid))
      "after" - TestDb.check(GetProjectEventLogic.after(pid, 2))
      "set1"  - TestDb.check(GetProjectEventLogic.set(pid, NonEmptySet(2)))
      "set2"  - TestDb.check(GetProjectEventLogic.set(pid, NonEmptySet(2, 3)))
    }

    "saveProjectEvent" - {
      "insertEventQuery" - TestDb.check(SaveProjectEventLogic.insertEventQuery)
      "updateProjectN"   - TestDb.check(SaveProjectEventLogic.updateProjectN)
      "updateProjectR"   - TestDb.check(SaveProjectEventLogic.updateProjectR)
    }

    "homeSpa" - {
      "createProject"                - TestDb.check(DbInterpreter.ForHomeSpa.createProjectQuery)
      "getAllProjectMetaDataForUser" - TestDb.check(db.getAllProjectMetaDataForUserQuery)
    }

    "projectSpa" - {
      "projectSpaInitPage" - TestDb.check(db.projectSpaInitPageQuery)
    }

    "ops" - {
      val db = new ForOps("blah")
      "nowSql"                 - TestDb.check(db.nowSql)
      "userStatsSql"           - TestDb.check(db.userStatsSql)
      "tableStatsSql"          - TestDb.check(db.tableStatsSql)
      "dbSizeSql"              - TestDb.check(db.dbSizeSql)
      "userIdByUsernameSql"    - TestDb.check(db.userIdByUsernameSql)
      "userIdByEmailSql"       - TestDb.check(db.userIdByEmailSql)
      "insertVerifiedEventSql" - TestDb.check(db.insertVerifiedEventSql)
    }

  }
}
