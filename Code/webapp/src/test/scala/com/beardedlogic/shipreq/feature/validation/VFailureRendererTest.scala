package shipreq.webapp.feature.validation

import org.scalatest.{Matchers, FunSpec}
import scala.xml.Text
import scalaz.NonEmptyList
import scalaz.syntax.semigroup._
import VFailure.semigroup

class VFailureRendererTest extends FunSpec with Matchers {

  val singleField = VFailure.forField("Car", NonEmptyList("is too big."))
  val multiField = VFailure.forField("Car", NonEmptyList("is too fast.", "is too big."))
  val singleLoose = VFailure.looseMsg("It's Tuesday.")
  val multiLoose = singleLoose |+| VFailure.looseMsg("It's too hot.")
  val multiTypes = singleLoose |+| singleField
  val multiTypes4 = multiLoose |+| multiField

  describe("Rendering to html") {
    it("Single field error") {
      singleField.toHtml shouldBe Text("Car is too big.")
    }
    it("Multiple field errors") {
      multiField.toHtml shouldBe Text("Car") ++ <ul><li>is too fast.</li><li>is too big.</li></ul>
    }
    it("Single loose error") {
      singleLoose.toHtml shouldBe Text("It's Tuesday.")
    }
    it("Multiple loose error") {
      multiLoose.toHtml shouldBe <ul><li>It's too hot.</li><li>It's Tuesday.</li></ul>
    }
    it("Different error types 1") {
      multiTypes.toHtml shouldBe <ul><li>It's Tuesday.</li><li>Car is too big.</li></ul>
    }
    it("Different error types 2") {
      multiTypes4.toHtml shouldBe <ul><li>It's too hot.</li><li>It's Tuesday.</li><li>Car<ul><li>is too fast.</li><li>is too big.</li></ul></li></ul>
    }
  }

  describe("Rendering to text") {
    it("Single field error") {
      singleField.toText shouldBe "Car is too big."
    }
    it("Multiple field errors") {
      multiField.toText shouldBe "Car\n  - is too fast.\n  - is too big."
    }
    it("Single loose error") {
      singleLoose.toText shouldBe "It's Tuesday."
    }
    it("Multiple loose error") {
      multiLoose.toText shouldBe "It's too hot.\n\nIt's Tuesday."
    }
    it("Different error types 1") {
      multiTypes.toText shouldBe "It's Tuesday.\n\nCar is too big."
    }
    it("Different error types 2") {
      multiTypes4.toText shouldBe "It's too hot.\n\nIt's Tuesday.\n\nCar\n  - is too fast.\n  - is too big."
    }
  }
}
