package shipreq.webapp.server.test

import shipreq.webapp.server.logic.test._

trait WebappServerTestEquality extends WebappServerLogicTestEquality

trait WebappServerTestUtil extends WebappServerLogicTestUtil

object WebappServerTestUtil
  extends WebappServerTestEquality
     with WebappServerTestUtil
