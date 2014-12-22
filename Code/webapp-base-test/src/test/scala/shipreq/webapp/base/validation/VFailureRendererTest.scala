package shipreq.webapp.base.validation

import utest._
import scalaz.NonEmptyList
import scalaz.syntax.semigroup._
import VFailure.semigroup

object VFailureRendererTest extends TestSuite {
  override def tests = TestSuite {

    val singleField = VFailure.forField("Car", NonEmptyList("is too big."))
    val multiField = VFailure.forField("Car", NonEmptyList("is too fast.", "is too big."))
    val singleLoose = VFailure.looseMsg("It's Tuesday.")
    val multiLoose = singleLoose |+| VFailure.looseMsg("It's too hot.")
    val multiTypes = singleLoose |+| singleField
    val multiTypes4 = multiLoose |+| multiField
  
    "Rendering to text" -{
      "Single field error" -{
        assert(singleField.toText == "Car is too big.")
      }
      "Multiple field errors" -{
        assert(multiField.toText == "Car\n  - is too fast.\n  - is too big.")
      }
      "Single loose error" -{
        assert(singleLoose.toText == "It's Tuesday.")
      }
      "Multiple loose error" -{
        assert(multiLoose.toText == "It's too hot.\n\nIt's Tuesday.")
      }
      "Different error types 1" -{
        assert(multiTypes.toText == "It's Tuesday.\n\nCar is too big.")
      }
      "Different error types 2" -{
        assert(multiTypes4.toText == "It's too hot.\n\nIt's Tuesday.\n\nCar\n  - is too fast.\n  - is too big.")
      }
    }
  }
}
