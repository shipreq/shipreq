package shipreq.webapp.server.db

import shipreq.base.test.db.{ImperativeXA, TestDb}
import shipreq.webapp.member.social._
import shipreq.webapp.server.logic.laws.DbUserGroupLaws
import shipreq.webapp.server.test._

object DbUserGroupTest extends DbUserGroupLaws {
  import UserGroup._

  override protected def beforeTest(): Unit =
    PrepareEnv.dbOnce()

  override protected def newDbApi[A](f: DbApi => A): A =
    TestDb.withImperativeXA { xa =>
      val db = new DbInstance()(xa)
      f(db)
    }

  private object DB extends DbInterpreter.ForHomeSpa with DbInterpreter.Base {
    override def getUserGroupUniverseU(id: Id) =
      super.getUserGroupUniverseU(id)
  }

  private final class DbInstance(implicit val xa: ImperativeXA) extends DbApi {
    private val dbu = DbUtil(xa)

    override def createUser()                = dbu.newUser()
    override val createUserGroup             = xa ! DB.createUserGroup(_, _, _)
    override val updateUserGroup             = xa ! DB.updateUserGroup(_, _, _, _, _)
    override val getUserGroupUniverseU       = xa ! DB.getUserGroupUniverseU(_)
    override val getUserGroupUniverseForUser = xa ! DB.getUserGroupUniverseForUser(_)
    override val getUserIdsByUsername        = xa ! DB.getUserIdsByUsername(_)
 }
}
