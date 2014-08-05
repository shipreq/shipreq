package shipreq.taskman.api.impl

import org.specs2.mutable.Specification
import scala.slick.jdbc.StaticQuery.queryNA
import shipreq.base.test.specs2.db.DatabaseTest
import shipreq.taskman.api.ApiOp
import ApiOp.CfgPut

class CfgTest extends Specification with DatabaseTest with ApiImplTestHelpers {

  "CfgPut" should {

    lazy val q = queryNA[(String,String)]("select k,v from cfg where k in ('a','b')")

    "insert new" in {
      run_(CfgPut("a", "start"), CfgPut("b", "omg"))
      q.list ==== List(("a", "start"), ("b", "omg"))
    }

    "update existing" in {
      run_(CfgPut("a", "start"), CfgPut("b", "omg"), CfgPut("a", "heheh"))
      q.list ==== List(("a", "heheh"), ("b", "omg"))
    }
  }

}