package shipreq.webapp.server.logic.test.test

import shipreq.webapp.server.logic.laws.DbUserGroupLaws
import shipreq.webapp.server.logic.test._

object MockDbUserGroupTest extends DbUserGroupLaws {

  override protected def newDbApi[A](f: DbApi => A): A =
    f(DbInstance)

  private object DbInstance extends DbApi {
    private val db = MockDb.withLiveClock()

    override def createUser()                = db.newUser()
    override val createUserGroup             = db.createUserGroup(_, _, _).value
    override val updateUserGroup             = db.updateUserGroup(_, _, _, _, _).value
    override val getUserGroupUniverseU       = db.getUserGroupUniverseU(_)
    override val getUserGroupUniverseForUser = db.getUserGroupUniverseForUser(_).value
    override val getUserIdsByUsername        = db.getUserIdsByUsername(_).value
 }
}
