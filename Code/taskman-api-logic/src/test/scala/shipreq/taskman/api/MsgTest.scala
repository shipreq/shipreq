package shipreq.taskman.api

import utest._
import japgolly.microlibs.testutil.TestUtil._
import scalaz.std.AllInstances._

object MsgTest extends TestSuite {

  override def tests = Tests {

    // TODO Use adt-macros
    //    "have as many values as Msg" in {
    //      MsgType.values must have size(allMsgs.size)
    //    }

    // TODO Prove by compilation or reenable this test
    //    "have an entry in #values for each type" in {
    //      val cs: List[Class[_ <: MsgType]] = MsgType.values.map(_.getClass)
    //      cs must containTheSameElementsAs(allMsgTypes.classes_.toList)
    //    }

    'uniqueIds {
      val ids = MsgType.values.map(_.id).sorted
      assertEq(ids, ids.distinct.sorted)
    }

  }
}
