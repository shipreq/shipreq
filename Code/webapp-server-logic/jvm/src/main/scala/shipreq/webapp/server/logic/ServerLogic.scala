package shipreq.webapp.server.logic

import scalaz.{Monad, ~>}

/**
  * All server logic.
  */
final case class ServerLogic[F[_]](publicSpa    : PublicSpaLogic[F],
                                   homeSpa      : HomeSpaLogic  [F],
                                   projectServer: ProjectServer [F])

object ServerLogic {

  def create[D[_] : DB.Algebra : Monad,
             F[_] : ProjectServer.StoreAlgebra : Server.Algebra : Monad]
            (b: ProjectServer.BroadcastTo)
            (implicit runDB: D ~> F)
            : ServerLogic[F] =
    ServerLogic(
      PublicSpaLogic[D, F],
      HomeSpaLogic[D, F],
      ProjectServer[D, F](b))
}