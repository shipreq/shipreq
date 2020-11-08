package shipreq.webapp.server.logic.impl

import scalaz.{BindRec, Catchable, Monad, ~>}
import shipreq.base.ops.Trace
import shipreq.taskman.api.TaskmanApi
import shipreq.webapp.server.logic.algebra._
import shipreq.webapp.server.logic.config.ServerLogicConfig
import shipreq.webapp.server.logic.event.ApplyEventAlgebra

/**
  * All server logic.
  */
final case class ServerLogic[F[_]](common    : CommonProtocolLogic[F],
                                   publicSpa : PublicSpaLogic     [F],
                                   homeSpa   : HomeSpaLogic       [F],
                                   projectSpa: ProjectSpaLogic    [F])

object ServerLogic {

  def create[D[_] : Monad
                  : DB.Algebra,
             F[_] : ApplyEventAlgebra
                  : Catchable
                  : MetricsAlgebra
                  : Redis.ProjectAlgebra
                  : Security.Algebra
                  : Server.Algebra
                  : TaskmanApi
                  : Trace.Algebra]
            (implicit F: Monad[F] with BindRec[F],
             runDB  : D ~> F,
             config : ServerLogicConfig,
             cryptoD: Crypto[D],
             cryptoF: Crypto[F]): ServerLogic[F] = {

    implicit val common = CommonProtocolLogic[F]
    implicit val assetManifest = config.assetManifest
    implicit val scalaJsManifest = config.scalaJsManifest

    ServerLogic(
      common,
      PublicSpaLogic [D, F],
      HomeSpaLogic   [D, F],
      ProjectSpaLogic[D, F](config.projectSpa))
  }
}