package shipreq.webapp.server.db

import doobie._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.test.db.{ImperativeXA, TestDb}
import shipreq.webapp.base.data.{ProjectId, UserId}
import shipreq.webapp.member.project.data.ProjectAccess
import shipreq.webapp.server.logic.laws.DbLaws
import shipreq.webapp.server.logic.util.Obfuscators
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
       with DbInterpreter.ForProjectSpa {

    val needProjectCreator = Query[ProjectId, UserId]("select creator_id from project where id=?")

    def getProjectAccess(id: ProjectId) =
      DbInterpreter.getProjectAccessQuery.toMap(id)
        .map(_.mapKeysNow(Obfuscators.userId.obfuscate))
        .map(ProjectAccess.apply)
  }

  private final class DbInstance(implicit val xa: ImperativeXA) extends DbApi {
    private val dbu = DbUtil(xa)

    override def createUser()         = dbu.newUser()
    override val getUsernamesByUserId = xa ! DB.getUsernamesByUserId(_)
    override val getUserIdsByUsername = xa ! DB.getUserIdsByUsername(_)
    override val createProject        = xa ! DB.createProject(_, _, _, _)
    override val getProjectAccess     = xa ! DB.getProjectAccess(_)
    override val projectSpaInitPage   = xa ! DB.projectSpaInitPage(_, _)
    override val getProjectRolodex    = xa ! DB.getProjectRolodex(_)
    override val needProjectCreator   = xa ! DB.needProjectCreator.unique(_)
    override val getProjectEvents     = xa ! DB.getProjectEvents(_)
    override val saveProjectEvent     = xa ! DB.saveProjectEvent(_, _, _, _, _)
  }
}
