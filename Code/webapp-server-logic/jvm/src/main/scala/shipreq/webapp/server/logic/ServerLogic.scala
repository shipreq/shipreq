package shipreq.webapp.server.logic

import scalaz.{BindRec, Monad, ~>}
import shipreq.base.ops.Trace
import shipreq.taskman.api.TaskmanApi
import shipreq.webapp.server.ServerConfig

/**
  * All server logic.
  */
final case class ServerLogic[F[_]](publicSpa    : PublicSpaLogic [F],
                                   homeSpa      : HomeSpaLogic   [F],
                                   projectServer: ProjectServer  [F],
                                   projectSpa   : ProjectSpaLogic[F])

object ServerLogic {

  def create[D[_] : Monad
                  : DB.Algebra,
             F[_] : MetricsLogic
                  : ProjectServer.StoreAlgebra
                  : Redis.ProjectAlgebra
                  : Security.Algebra
                  : Server.Algebra
                  : TaskmanApi
                  : Trace.Algebra]
            (b: ProjectServer.BroadcastTo)
            (implicit F: Monad[F] with BindRec[F],
             runDB: D ~> F,
             config: ServerConfig): ServerLogic[F] =
    ServerLogic(
      PublicSpaLogic [D, F],
      HomeSpaLogic   [D, F],
      ProjectServer  [D, F](b),
      ProjectSpaLogic[D, F])
}