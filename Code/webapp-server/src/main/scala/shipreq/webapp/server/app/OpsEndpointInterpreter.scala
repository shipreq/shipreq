package shipreq.webapp.server.app

import nyaya.gen.Gen
import shipreq.base.util.FxModule._
import shipreq.taskman.api.TaskmanApi
import shipreq.webapp.server.logic.algebra.{DB, Server}
import shipreq.webapp.server.logic.impl.OpsEndpointLogic

final class OpsEndpointInterpreter(implicit
                                   db: DB.ForOps[Fx],
                                   svr: Server.Time[Fx],
                                   taskman: TaskmanApi[Fx])
  extends OpsEndpointLogic.Base[Fx]()(implicitly, db, svr, taskman) {

  override protected val randomToken =
    Fx(Gen.alphaNumeric.string(16).sample())

}
