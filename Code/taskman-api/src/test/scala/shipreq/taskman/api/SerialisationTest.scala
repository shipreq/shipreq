package shipreq.taskman.api

import org.specs2.mutable._
import org.specs2.ScalaCheck
import scalaz.{-\/, \/-}
import TestHelpers._
import Serialisation._
import Types._

class SerialisationTest extends Specification with ScalaCheck {

  "Serialisation" should {
    "serialise and deserialise back" ! prop{ (t: TaskDef) =>
      deser(TaskTypes.lookupType(t).id, ser(t)) ==== \/-(t)
    }

    "return an error for unknown task types" in {
      deser(-500, "".tag) must beLike{ case -\/(e) if e contains "-500" => ok }
    }

    "return an error if data fails parsing" in {
      deser(TaskType.RegistrationRequested.id, """{"x":1}""".tag) must beLike{ case -\/(e) => ok }
    }
  }
}
