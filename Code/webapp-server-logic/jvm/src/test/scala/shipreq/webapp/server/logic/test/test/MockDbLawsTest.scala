package shipreq.webapp.server.logic.test.test

import shipreq.webapp.server.logic.laws.DbLaws
import shipreq.webapp.server.logic.test._

object MockDbLawsTest extends DbLaws {

  override protected def newDbApi[A](f: DbApi => A): A =
    f(DbInstance)

  private object DbInstance extends DbApi {
    private val db = MockDb.withLiveClock()

    override def createUser()         = db.newUser()
    override val getUserIdsByUsername = db.getUserIdsByUsername(_).value
    override val createProject        = db.createProject(_, _, _, _).value
    override val updateProjectAccess  = db.updateProjectAccess(_, _, _).value
    override val getProjectAccess     = db.getProjectAccess(_).value
    override val projectSpaInitPage   = db.projectSpaInitPage(_, _).value
 }
}
