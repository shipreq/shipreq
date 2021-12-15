package shipreq.webapp.server.db

import shipreq.base.test.db.{ImperativeXA, TestDb}
import shipreq.webapp.server.logic.laws.DbLaws
import shipreq.webapp.server.test._

object DbLawsTest extends DbLaws {

  override protected def beforeTest(): Unit =
    PrepareEnv.dbOnce()

  override protected def newDbApi[A](f: DbApi => A): A =
    TestDb.withImperativeXA { xa =>
      val db = new DbInstance()(xa)
      f(db)
    }

  private object DB
    extends DbInterpreter.Base
       with DbInterpreter.ForHomeSpa
       with DbInterpreter.ForProjectSpa

  private final class DbInstance(implicit val xa: ImperativeXA) extends DbApi {
    private val dbu = DbUtil(xa)

    override def createUser()         = dbu.newUser()
    override val getUserIdsByUsername = xa ! DB.getUserIdsByUsername(_)
 }
}
