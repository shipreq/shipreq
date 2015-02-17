package shipreq.webapp.client.app.ui.reqtable

import utest._

object LogicTest extends TestSuite {


  // project > gather > rows

  // properties
  // ==========
  // no dups
  // all present - set row.id = set project._.id
  // expansions per req
  // - sum codes = req.codes
  // - if req.codes then no rows without codes

  override def tests = TestSuite {

  }
}
