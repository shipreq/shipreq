package shipreq.webapp.server.interpreter

import nyaya.gen.Gen
import shipreq.base.util.FxModule._
import shipreq.taskman.api.TaskmanApi
import shipreq.webapp.server.logic.algebra.{Crypto, DB, Server}
import shipreq.webapp.server.logic.logic.OpsEndpointLogic

final class OpsEndpointInterpreter(implicit
                                   crypto: Crypto[Fx],
                                   db: DB.ForOps[Fx],
                                   svr: Server.Time[Fx],
                                   taskman: TaskmanApi[Fx])
  extends OpsEndpointLogic.Base[Fx]()(implicitly, crypto, db, svr, taskman) {

  override protected val randomToken =
    Fx(Gen.alphaNumeric.string(16).sample())

}
