package shipreq.taskman.api.impl

import org.specs2.mutable._
import org.specs2.ScalaCheck
import scalaz.{-\/, \/-}
import shipreq.taskman.api.Types._
import shipreq.taskman.api.TestHelpers._
import shipreq.taskman.api.{Msg, MsgType}
import Serialisation._

class SerialisationTest extends Specification with ScalaCheck {

  "Serialisation" should {
    "serialise and deserialise back" ! prop{ (m: Msg) =>
      deserialise(MsgType.lookup(m).id, serialise(m)) ==== \/-(m)
    }

    "return an error for unknown task types" in {
      deserialise(-500.toShort, "".tag) must beLike{ case -\/(e) if e.toString contains "-500" => ok }
    }

    "return an error if data fails parsing" in {
      deserialise(MsgType.RegistrationRequested.id, """{"x":1}""".tag) must beLike{ case -\/(_) => ok }
    }
  }
}
