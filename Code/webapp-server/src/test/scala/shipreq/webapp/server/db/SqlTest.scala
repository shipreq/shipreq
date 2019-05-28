package shipreq.webapp.server.db

import utest._
import shipreq.base.test.db.SqlTester.test
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.base.event.EventOrd
import shipreq.webapp.server.logic.DB
import shipreq.webapp.server.test.PrepareEnv

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
      val pid = ProjectId(123)
      val o2 = EventOrd(2)
      val o3 = EventOrd(3)
      'createEmptyProjectSql           - test(db.createEmptyProjectSql)
      'getAllProjectMetaDataForUserSql - test(db.getAllProjectMetaDataForUserSql)
      'getProjectMetaDataSql           - test(db.getProjectMetaDataSql)
      'sqlSelectEventsAll              - test(db.SqlSelectEvents.all.toQuery0(pid))
      'sqlSelectEventsExcludeUpTo      - test(db.SqlSelectEvents.after.toQuery0((pid, o2)))
      'sqlSelectEventsSet              - test(db.SqlSelectEvents.setQuery(Seq(o2)).toQuery0(pid))
      'sqlSelectEventsSet              - test(db.SqlSelectEvents.setQuery(Seq(o2, o3)).toQuery0(pid))
      'sqlSelectAllEventHashes         - test(db.sqlSelectAllEventHashes)
      'projectSpaInitPageSql           - test(db.projectSpaInitPageSql)
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
