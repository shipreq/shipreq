package shipreq.webapp.server.security

import org.apache.shiro.authc._
import utest._
import shipreq.webapp.server.test.UserFixture
import shipreq.webapp.server.test.WebappServerTestUtil._

object OshiroTest extends TestSuite {

  def runTest[A](test: UserFixture => A): A =
    UserFixture.Transaction.runNow(withOshiro(test))

  override def tests = TestSuite {

    'Authentication {
      'allowUsername - runTest { uf =>
        login(uf.user1.username.value, uf.user1.password)
      }

      'allowEmail - runTest { uf =>
        login(uf.user1.email.value, uf.user1.password)
      }

      'notFound - runTest { uf =>
        intercept[UnknownAccountException](login("blah", uf.user1.password))
      }

      'badPassword - runTest { uf =>
        intercept[IncorrectCredentialsException](login(uf.user1.username.value, uf.user2.password))
      }

      'unregistered - runTest { uf =>
        intercept[UnknownAccountException](login(uf.userWithCurrentToken.email.value, ""))
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
