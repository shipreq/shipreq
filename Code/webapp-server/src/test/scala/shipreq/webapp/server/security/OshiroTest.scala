package shipreq.webapp.server
package security

import org.apache.shiro.authc._
import utest._
import shipreq.webapp.server.test.UserFixture
import shipreq.webapp.server.test.WebappServerTestUtil._

object OshiroTest extends TestSuite {

  def inEnv[A](f: UserFixture => A): A =
    withOshiro(UserFixture.Session(f))

  override def tests = TestSuite {

    'Authentication {
      'allowUsername - inEnv { uf =>
        login(uf.user1.username.value, uf.user1.password)
      }

      'allowEmail - inEnv { uf =>
        login(uf.user1.email.value, uf.user1.password)
      }

      'notFound - inEnv { uf =>
        intercept[UnknownAccountException](login("blah", uf.user1.password))
      }

      'badPassword - inEnv { uf =>
        intercept[IncorrectCredentialsException](login(uf.user1.username.value, uf.user2.password))
      }

      'unregistered - inEnv { uf =>
        intercept[UnknownAccountException](login(uf.userWithCurrentToken.email.value, ""))
      }
    }

    'loggedInUser {
      'anon - inEnv(_ =>
        assertNotLoggedIn())

      'loggedIn - inEnv { uf =>
        login(uf.user1.username.value, uf.user1.password)
        assertUserLoggedIn(uf.user1.toUserDescriptor)
      }
    }

  }
}
