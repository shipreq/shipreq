package shipreq.taskman.api.impl

import doobie._
import utest._
import shipreq.base.test.db.TestDb

object CfgTest extends TestSuite {

  override def tests = Tests {
    val api = TaskmanApiImpl(TestDb.db.schema)
    val q = Query0[(String, String)]("select k,v from cfg where k in ('a','b') order by 1")

    "cfgPut" - {

      "insert" - {
        val result = TestDb ! (
          for {
            _ <- api.cfgPut("a", "start")
            _ <- api.cfgPut("b", "omg")
            r <- q.to[List]
          } yield r
        )
        assert(result == List(("a", "start"), ("b", "omg")))
      }

      "update" - {
        val result = TestDb ! (
          for {
            _ <- api.cfgPut("a", "start")
            _ <- api.cfgPut("b", "omg")
            _ <- api.cfgPut("a", "heheh")
            r <- q.to[List]
          } yield r
        )
        assert(result == List(("a", "heheh"), ("b", "omg")))
      }
    }

  }
}
