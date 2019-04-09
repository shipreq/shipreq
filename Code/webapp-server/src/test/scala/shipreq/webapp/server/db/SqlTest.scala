package shipreq.webapp.server.db

import utest._
import shipreq.base.test.db.SqlTester.test
import shipreq.webapp.server.test.PrepareEnv

object SqlTest extends TestSuite {
  import DbInterpreter._

  private lazy val db = new DbInterpreter()(PrepareEnv.global().config.security)

  override def tests = Tests {

    'security {
      val db = ForSecurity
      'getUserAndPasswordByEmailSql    - test(db.getUserAndPasswordByEmailSql)
      'getUserAndPasswordByUsernameSql - test(db.getUserAndPasswordByUsernameSql)
      'logLoginSuccessSql              - test(db.logLoginSuccessSql)
      'getProjectOwnerSql              - test(db.getProjectOwnerSql)
    }

    'securityTokenReadOnly {
      val db = SecurityTokenReadOnly
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

    'saveProjectEvent {
      'insertEventSql     - test(db.insertEventSql)
      'insertEventHashSql - test(db.insertEventHashSql)
    }

    'members {
      'createEmptyProjectSql           - test(db.createEmptyProjectSql)
      'getAllProjectMetaDataForUserSql - test(db.getAllProjectMetaDataForUserSql)
      'getProjectMetaDataSql           - test(db.getProjectMetaDataSql)
      'getProjectHeaderSql             - test(db.getProjectHeaderSql)
      'sqlSelectAllEvents              - test(db.sqlSelectAllEvents)
      'sqlSelectAllEventHashes         - test(db.sqlSelectAllEventHashes)
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
