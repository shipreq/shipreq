package shipreq.webapp.security

import org.scalatest.{Matchers, FunSpec}
import shipreq.taskman.api.UserId
import shipreq.webapp.db.UserDescriptor

class PermissionsTest extends FunSpec with Matchers {

  val admin = UserDescriptor(UserId(1), "ad", "ad@ad.com", Set(Roles.Admin.name))
  val joe = UserDescriptor(UserId(2), "joe", "joe@ad.com", Set.empty)

  describe("admin") {
    it("should allow admin") {
      Permissions.admin.using(user = Some(admin)).isPass shouldBe true
    }
    it("should deny anon") {
      Permissions.admin.using(user = None).isPass shouldBe false
    }
    it("should deny normal users") {
      Permissions.admin.using(user = Some(joe)).isPass shouldBe false
    }
  }
}
