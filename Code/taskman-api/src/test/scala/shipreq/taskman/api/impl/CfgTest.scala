package shipreq.taskman.api.impl

import org.specs2.mutable.Specification
import doobie.imports._
import shipreq.base.test.specs2.db.DatabaseTest

class CfgTest extends Specification with DatabaseTest with ApiImplTestHelpers {

  "_.cfgPut" should {

    lazy val q = Query0[(String,String)]("select k,v from cfg where k in ('a','b')")

    "insert new" in {
      run_(_.cfgPut("a", "start"), _.cfgPut("b", "omg"))
      q.list.runNow() ==== List(("a", "start"), ("b", "omg"))
    }

    "update existing" in {
      run_(_.cfgPut("a", "start"), _.cfgPut("b", "omg"), _.cfgPut("a", "heheh"))
      q.list.runNow() ==== List(("a", "heheh"), ("b", "omg"))
    }
  }

}