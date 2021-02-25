package shipreq.webapp.server

import shipreq.base.db.scalazDoobieConnectionIO
import shipreq.webapp.base.data.UserId
import shipreq.webapp.server.logic.config.ProjectAccessHacks
import shipreq.webapp.server.logic.impl.HomeSpaLogic
import shipreq.webapp.server.logic.util.Obfuscators
import shipreq.webapp.server.test.WebappServerTestUtil._
import shipreq.webapp.server.test._
import sourcecode.Line
import utest._

object ProjectAccessHacksTest extends TestSuite {

  override def tests = Tests {

    val noHacks = ProjectAccessHacks.empty

    "home" - UserFixture.use { uf =>
      import uf.xa
      implicit val db = uf.dbUtil.dbAlgebra

      val u1 = uf.user1.id
      val u2 = uf.user2.id
      val p1 = Obfuscators.projectId.deobfuscate((xa ! HomeSpaLogic.createProject(u1, "X")).id).getOrThrow()
      val p2 = Obfuscators.projectId.deobfuscate((xa ! HomeSpaLogic.createProject(u2, "Y")).id).getOrThrow()

      def assertResults(u: UserId, hacks: ProjectAccessHacks)(expectedProjectIds: Long*)(implicit l: Line): Unit = {
        val results = xa ! db.getAllProjectMetaDataForUser(u, hacks)
        val actual = results.iterator.map(_.id).map(Obfuscators.projectId.deobfuscate(_).getOrThrow().value).toSet
        val expect = expectedProjectIds.toSet
        assertSet(actual, expect = expect)
      }

      assertResults(u1, noHacks)(p1)
      assertResults(u2, noHacks)(p2)

      val hacks = ProjectAccessHacks.config.parse(s"""{ "${u2.value}":[${p1.value}] }""").getOrThrow()
      assertResults(u1, hacks)(p1)
      assertResults(u2, hacks)(p2, p1)
    }

  }
}
