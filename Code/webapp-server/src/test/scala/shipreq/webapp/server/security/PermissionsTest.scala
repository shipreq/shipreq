package shipreq.webapp.server.security

import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.webapp.base.data._
import shipreq.webapp.server.data._

object PermissionsTest extends TestSuite {
  implicit def autoUsername(a: String) = Username(a)
  implicit def autoEmailAddr(a: String) = EmailAddr(a)

  val admin = UserDescriptor(UserId(1), "ad", "ad@ad.com", Set(Roles.Admin.name))
  val joe = UserDescriptor(UserId(2), "joe", "joe@ad.com", Set.empty)

  override def tests = TestSuite {
    'admin {
      'admin  - assertEq(Permissions.admin.using(user = Some(admin)).isPass, true)
      'anon   - assertEq(Permissions.admin.using(user = None       ).isPass, false)
      'normal - assertEq(Permissions.admin.using(user = Some(joe)  ).isPass, false)
    }
  }
}
