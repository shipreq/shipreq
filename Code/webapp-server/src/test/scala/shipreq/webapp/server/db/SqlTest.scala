package shipreq.webapp.server.db

import utest._
import shipreq.base.test.db.SqlTester.test
import shipreq.webapp.server.test.PrepareEnv
import shipreq.webapp.base.test.UnsafeTypes._

object SqlTest extends TestSuite {
  import DbInterpreter._

  private lazy val db = new DbInterpreter()(PrepareEnv.global().config.server.security)

  override def tests = Tests {

    'security {
      val db = ForSecurity
      'getUserAndPasswordByEmailSql    - test(db.getUserAndPasswordByEmailSql)
      'getUserAndPasswordByUsernameSql - test(db.getUserAndPasswordByUsernameSql)
      'logLoginSuccessSql              - test(db.logLoginSuccessSql)
      'getProjectOwnerSql              - test(db.getProjectOwnerSql)
    }

    'verificationTokenReadOnly {
      val db = VerificationTokenReadOnly
      'getUserRegistrationTokenIssueDateSql - test(db.getUserRegistrationTokenIssueDateSql)
      'getResetPasswordTokenIssueDateSql    - test(db.getResetPasswordTokenIssueDateSql)
    }

    'publicSpa {
      'getUserRegistrationSql               - test(db.getUserRegistrationSql)
      'createUserPlaceholderSql             - test(db.createUserPlaceholderSql)
      'updateUserRegistrationTokenSql       - test(db.updateUserRegistrationTokenSql)
      'sqlRegisterUser                      - test(db.sqlRegisterUser)
      'sqlInsertUsrd                        - test(db.sqlInsertUsrd)
      'getPasswordResetStateByEmailSql      - test(db.getPasswordResetStateByEmailSql)
      'getPasswordResetStateByUsernameSql   - test(db.getPasswordResetStateByUsernameSql)
      'createResetPasswordTokenSql          - test(db.createResetPasswordTokenSql)
      'updateResetPasswordTokenOnReissueSql - test(db.updateResetPasswordTokenOnReissueSql)
      'updateUserPasswordSql                - test(db.updateUserPasswordSql)
    }

    'getProjectMetaData {
      'projectMetaDataQuery - test(db.getProjectMetaDataQuery)
    }

    'getProjectEvents {
      'all   - test(GetProjectEventLogic.all)
      'after - test(GetProjectEventLogic.after)
      'set1  - test(GetProjectEventLogic.setSubset(Seq(2)))
      'set2  - test(GetProjectEventLogic.setSubset(Seq(2, 3)))
    }

    'saveProjectEvent {
      'insertEventQuery - test(SaveProjectEventLogic.insertEventQuery)
      'updateProjectN   - test(SaveProjectEventLogic.updateProjectN)
      'updateProjectR   - test(SaveProjectEventLogic.updateProjectR)
    }

    'homeSpa {
      'createProject                - test(db.createProjectQuery)
      'getAllProjectMetaDataForUser - test(db.getAllProjectMetaDataForUserQuery)
    }

    'projectSpa {
      'projectSpaInitPage - test(db.projectSpaInitPageQuery)
    }

    'ops {
      val db = new ForOps("blah")
      'nowSql        - test(db.nowSql)
      'userStatsSql  - test(db.userStatsSql)
      'tableStatsSql - test(db.tableStatsSql)
      'dbSizeSql     - test(db.dbSizeSql)
    }

  }
}
