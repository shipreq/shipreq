package shipreq.webapp.security

import shipreq.webapp.lib.Types._
import org.scalatest.{Matchers, FunSpec}
import shipreq.webapp.db.UserDescriptor

class PermissionsTest extends FunSpec with Matchers {

  val admin = UserDescriptor(1.tag, "ad", "ad@ad.com", Set(Roles.Admin.name))
  val joe = UserDescriptor(2.tag, "joe", "joe@ad.com", Set.empty)

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
