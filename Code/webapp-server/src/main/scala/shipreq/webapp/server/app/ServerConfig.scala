package shipreq.webapp.server.app

import japgolly.clearconfig._
import monocle.macros.Lenses
import scalaz.syntax.applicative._
import shipreq.base.db.DbConfig
import shipreq.webapp.server.ServerLogicConfig
import shipreq.webapp.server.redis.RedissonConfig

@Lenses
final case class ServerConfig(db    : DbConfig,
                              redis : Option[RedissonConfig],
                              server: ServerLogicConfig)

object ServerConfig {

  def config: ConfigDef[ServerConfig] =
    ( DbConfig.config |@|
      RedissonConfig.config.withPrefix("redis.").option |@|
      ServerLogicConfig.config
    )(apply)

}