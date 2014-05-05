package shipreq.taskman.api

import org.specs2.mutable._
import org.specs2.ScalaCheck
import TestHelpers._

class MsgTest extends Specification with ScalaCheck {

  "MsgType" should {

    "have as many values as Msg" in {
      MsgType.values must have size(allMsgs.size)
    }

    "have an entry in #values for each type" in {
      val cs: List[Class[_ <: MsgType]] = MsgType.values.map(_.getClass)
      cs must containTheSameElementsAs(allMsgTypes.classes_.toList)
    }

    "have unique ids" in {
      val ids = MsgType.values.map(_.id)
      ids must containTheSameElementsAs(ids.distinct)
    }
  }
}
