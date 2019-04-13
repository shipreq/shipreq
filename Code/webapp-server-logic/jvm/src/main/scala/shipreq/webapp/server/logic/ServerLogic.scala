package shipreq.webapp.server.logic

import scalaz.{Monad, ~>}
import shipreq.base.ops.Trace
import shipreq.taskman.api.TaskmanApi
import shipreq.webapp.server.ServerConfig

/**
  * All server logic.
  */
final case class ServerLogic[F[_]](publicSpa    : PublicSpaLogic[F],
                                   homeSpa      : HomeSpaLogic  [F],
                                   projectServer: ProjectServer [F]) {

  def publicSpaDispatch: PublicSpaLogic.ForDispatch[F] =
    publicSpa
}

object ServerLogic {

  def create[D[_] : Monad
                  : DB.Algebra,
             F[_] : Monad
                  : MetricsLogic
                  : ProjectServer.StoreAlgebra
                  : Security.Algebra2
                  : Server.Algebra
                  : TaskmanApi
                  : Trace.Algebra]
            (b: ProjectServer.BroadcastTo)
            (implicit runDB: D ~> F, config: ServerConfig)
            : ServerLogic[F] =
    ServerLogic(
      PublicSpaLogic[D, F],
      HomeSpaLogic[D, F],
      ProjectServer[D, F](b))
}