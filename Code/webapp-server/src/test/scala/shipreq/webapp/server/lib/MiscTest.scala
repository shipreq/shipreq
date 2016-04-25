package shipreq.webapp.server.lib

import org.scalatest.FunSpec
import org.scalatest.Matchers
import net.liftweb.util.Helpers._

class MiscTest extends FunSpec with Matchers with Misc {

  describe("filterCovar()") {} // TODO

  describe("#randomConfirmationToken") {
    it("should return different values each time") {
      randomConfirmationToken should not be(randomConfirmationToken)
      randomConfirmationToken should not be(randomConfirmationToken)
      randomConfirmationToken should not be(randomConfirmationToken)
    }
  }
}
