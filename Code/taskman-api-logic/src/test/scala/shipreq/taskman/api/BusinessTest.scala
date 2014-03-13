package shipreq.taskman.api

import org.specs2.mutable._
import org.specs2.ScalaCheck
import TestHelpers._
import shipreq.taskman.api.{TaskType => T}
import shipreq.taskman.api.{TaskDef => D}
import TaskTypes._

class BusinessTest extends Specification with ScalaCheck {

  "TaskDefs" should {
    "be as many as there are TaskTypes" in {
      taskDefClassFullNames.size ==== TaskType.values.size
    }
    "all have a unique type" in {
      val defsWithIds = TaskType.values.toList.map(t => TaskTypes.lookupTaskDef(t).getSimpleName).toSet
      defsWithIds must containTheSameElementsAs(taskDefClassShortNames)
    }

    "be the same type after serialisation & deserialisation" ! prop{ (t: TaskDef) =>
      lookupTaskDef(lookupType_!(lookupType(t).id)).getCanonicalName == t.getClass.getCanonicalName
    }
  }
}
