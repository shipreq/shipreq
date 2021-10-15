package shipreq.webapp.server.config

import cats.syntax.apply._
import japgolly.clearconfig._
import monocle.macros.Lenses
import shipreq.base.db.DbConfig
import shipreq.webapp.server.db.StatRecorder
import shipreq.webapp.server.logic.config.ServerLogicConfig
import shipreq.webapp.server.redis.RedissonConfig
import shipreq.webapp.server.util.AnalyticsProxy

@Lenses
final case class ServerConfig(analyticsProxy: AnalyticsProxy.Config,
                              db            : DbConfig,
                              redis         : Option[RedissonConfig],
                              server        : ServerLogicConfig,
                              statcounter   : Option[ServerConfig.Statcounter],
                              statRecorder  : StatRecorder.Config)

object ServerConfig {

  final case class Statcounter(project: Int, security: String)

  object Statcounter {
    def config: ConfigDef[Statcounter] =
      ( ConfigDef.need[Int]("project"),
        ConfigDef.need[String]("security").secret,
      ).mapN(apply)
        .withPrefix("statcounter.")
  }

  def config: ConfigDef[ServerConfig] =
    ( AnalyticsProxy.config.withPrefix("shipreq."),
      DbConfig.config,
      RedissonConfig.config.withPrefix("redis.").option,
      ServerLogicConfig.config,
      Statcounter.config.withPrefix("shipreq.").option,
      StatRecorder.config.withPrefix("shipreq."),
    ).mapN(apply)

}