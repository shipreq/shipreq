package shipreq.webapp.server.security

import utest._
import shipreq.webapp.base.user.PlainTextPassword
import shipreq.webapp.server.test.UserFixture
import shipreq.webapp.server.test.WebappServerTestUtil._

object ShiroTest extends TestSuite {

  def runTest[A](test: UserFixture => A): Unit =
    UserFixture.Session.runNow(withShiro(test))

  override def tests = Tests {

    'Authentication {
      'allowUsername - runTest { uf =>
        login(uf.user1.username.value, uf.user1.password)
      }

      'allowEmail - runTest { uf =>
        login(uf.user1.email.value, uf.user1.password)
      }

      'notFound - runTest { uf =>
        intercept[RuntimeException](login("blah", uf.user1.password))
      }

      'badPassword - runTest { uf =>
        intercept[RuntimeException](login(uf.user1.username.value, uf.user2.password))
      }

      'unregistered - runTest { uf =>
        intercept[RuntimeException](login(uf.userWithCurrentToken.email.value, PlainTextPassword("")))
      }
    }

    'loggedInUser {
      'anon - runTest(_ =>
        assertNotLoggedIn())

      'loggedIn - runTest { uf =>
        login(uf.user1.username.value, uf.user1.password)
        assertUserLoggedIn(uf.user1.toUserDescriptor)
      }
    }

  }
}
