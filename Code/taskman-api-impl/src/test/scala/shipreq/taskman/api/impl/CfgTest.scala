package shipreq.taskman.api.impl

import org.specs2.mutable.Specification
import doobie.imports._
import shipreq.base.test.specs2.db.DatabaseTest
import shipreq.taskman.api.ApiOp
import ApiOp.CfgPut

class CfgTest extends Specification with DatabaseTest with ApiImplTestHelpers {

  "CfgPut" should {

    lazy val q = Query0[(String,String)]("select k,v from cfg where k in ('a','b')")

    "insert new" in {
      run_(CfgPut("a", "start"), CfgPut("b", "omg"))
      q.list.runNow() ==== List(("a", "start"), ("b", "omg"))
    }

    "update existing" in {
      run_(CfgPut("a", "start"), CfgPut("b", "omg"), CfgPut("a", "heheh"))
      q.list.runNow() ==== List(("a", "heheh"), ("b", "omg"))
    }
  }

}