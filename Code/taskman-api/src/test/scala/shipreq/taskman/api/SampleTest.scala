package shipreq.taskman.api

import org.specs2.mutable._
import org.specs2.ScalaCheck
import TestHelpers._
import shipreq.taskman.api.{TaskType => T}
import shipreq.taskman.api.{TaskDef => D}
import TaskTypes._

class XxxxTest extends Specification with ScalaCheck {

  "xxxxxxxxxxxxxxxxxxxxxx" should {
    "be as many as there are TaskTypes" in {
      taskDefClassFullNames.size ==== TaskType.values.size
    }
  }
}
