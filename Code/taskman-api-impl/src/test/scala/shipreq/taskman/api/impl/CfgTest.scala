package shipreq.taskman.api.impl

import org.specs2.mutable.Specification
import shipreq.base.test.db.specs2.DatabaseTest
import shipreq.taskman.api.ApiOp
import ApiOp.CfgPut

class CfgTest extends Specification with DatabaseTest with ApiImplTestHelpers {

  "CfgPut" should {

    "insert new" in {
      runApiOp(CfgPut("a", "start"), CfgPut("b", "omg"))
      sql"select k,v from cfg where k in ('a','b')".as[(String, String)].list ==== List(("a", "start"), ("b", "omg"))
    }

    "update existing" in {
      runApiOp(CfgPut("a", "start"), CfgPut("b", "omg"), CfgPut("a", "heheh"))
      sql"select k,v from cfg where k in ('a','b')".as[(String, String)].list ==== List(("a", "heheh"), ("b", "omg"))
    }
  }

}