package shipreq.webapp.server.redis

import cats.syntax.apply._
import japgolly.clearconfig._
import java.time.Duration
import org.redisson.config.{Config, SingleServerConfig}

final case class RedissonConfig(url: String,
                                extra1: Config => Unit,
                                extra2: SingleServerConfig => Unit) {

  lazy val instance: Config = {
    val cfg = new Config
    val s = cfg.useSingleServer().setAddress(url)
    extra1(cfg)
    extra2(s)
    cfg
  }
}

object RedissonConfig {

  private def extraDef1: ConfigDef[Config => Unit] = {
    val i = new Config
    ConfigDef.consumerFn[Config](
      _.get     ("password"    , _.setPassword    ),
      _.getOrUse("tcpNoDelay"  , _.setTcpNoDelay  )(i.isTcpNoDelay  ),
      _.getOrUse("tcpKeepAlive", _.setTcpKeepAlive)(i.isTcpKeepAlive),
    )
  }

  private def extraDef2: ConfigDef[SingleServerConfig => Unit] = {
    val i = (new Config).useSingleServer()
    ConfigDef.consumerFn[SingleServerConfig](
      _.getOrUse("clientName"                           , _.setClientName                           )("webapp"                                  ),
      _.getOrUse("connectionMinimumIdleSize"            , _.setConnectionMinimumIdleSize            )(i.getConnectionMinimumIdleSize            ),
      _.getOrUse("connectionPoolSize"                   , _.setConnectionPoolSize                   )(i.getConnectionPoolSize                   ),
      _.getOrUse("retryAttempts"                        , _.setRetryAttempts                        )(1                                         ),
      _.getOrUse("subscriptionConnectionMinimumIdleSize", _.setSubscriptionConnectionMinimumIdleSize)(i.getSubscriptionConnectionMinimumIdleSize),
      _.getOrUse("subscriptionConnectionPoolSize"       , _.setSubscriptionConnectionPoolSize       )(i.getSubscriptionConnectionPoolSize       ),
      _.getOrUse("subscriptionsPerConnection"           , _.setSubscriptionsPerConnection           )(i.getSubscriptionsPerConnection           ),
      // durations
      _.getOrUseC[Duration]("connectTimeout"        , (c, t) => c.setConnectTimeout        (t.toMillis.toInt))(Duration.ofMillis(i.getConnectTimeout        )),
      _.getOrUseC[Duration]("dnsMonitoringInterval" , (c, t) => c.setDnsMonitoringInterval (t.toMillis      ))(Duration.ofMillis(i.getDnsMonitoringInterval )),
      _.getOrUseC[Duration]("idleConnectionTimeout" , (c, t) => c.setIdleConnectionTimeout (t.toMillis.toInt))(Duration.ofMillis(i.getIdleConnectionTimeout )),
      _.getOrUseC[Duration]("pingConnectionInterval", (c, t) => c.setPingConnectionInterval(t.toMillis.toInt))(Duration.ofMillis(i.getPingConnectionInterval)),
      _.getOrUseC[Duration]("timeout"               , (c, t) => c.setTimeout               (t.toMillis.toInt))(Duration.ofMillis(800                        )),
    )
  }

  def config: ConfigDef[RedissonConfig] =
    ( ConfigDef.need[String]("url"),
      extraDef1,
      extraDef2,
    ).mapN(apply)
}
