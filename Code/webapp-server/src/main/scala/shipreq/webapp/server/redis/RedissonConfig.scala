package shipreq.webapp.server.redis

import cats.syntax.apply._
import japgolly.clearconfig._
import java.time.Duration
import org.redisson.config.{Config, SingleServerConfig}

final case class RedissonConfig(url: String,
                                extra: SingleServerConfig => Unit) {

  lazy val instance: Config = {
    val cfg = new Config
    val s = cfg.useSingleServer().setAddress(url)
    extra(s)
    cfg
  }
}

object RedissonConfig {

  private def extraDef = {
    val i = (new Config).useSingleServer()
    ConfigDef.consumerFn[SingleServerConfig](
      _.getOrUse("clientName"                            , _.setClientName                           )("webapp"                                  ),
      _.getOrUse("connectionMinimumIdleSize"             , _.setConnectionMinimumIdleSize            )(i.getConnectionMinimumIdleSize            ),
      _.getOrUse("connectionPoolSize"                    , _.setConnectionPoolSize                   )(i.getConnectionPoolSize                   ),
      _.getOrUse("keepAlive"                             , _.setKeepAlive                            )(i.isKeepAlive                             ),
      _.get     ("password"                              , _.setPassword                             )                                            ,
      _.getOrUse("retryAttempts"                         , _.setRetryAttempts                        )(1                                         ),
      _.getOrUse("subscriptionConnectionMinimumIdleSize" , _.setSubscriptionConnectionMinimumIdleSize)(i.getSubscriptionConnectionMinimumIdleSize),
      _.getOrUse("subscriptionConnectionPoolSize"        , _.setSubscriptionConnectionPoolSize       )(i.getSubscriptionConnectionPoolSize       ),
      _.getOrUse("subscriptionsPerConnection"            , _.setSubscriptionsPerConnection           )(i.getSubscriptionsPerConnection           ),
      _.getOrUse("tcpNoDelay"                            , _.setTcpNoDelay                           )(i.isTcpNoDelay                            ),
      // durations
      _.getOrUseC[Duration]("connectTimeout"         , (c, t) => c.setConnectTimeout        (t.toMillis.toInt))(Duration.ofMillis(i.getConnectTimeout        )),
      _.getOrUseC[Duration]("dnsMonitoringInterval"  , (c, t) => c.setDnsMonitoringInterval (t.toMillis      ))(Duration.ofMillis(i.getDnsMonitoringInterval )),
      _.getOrUseC[Duration]("idleConnectionTimeout"  , (c, t) => c.setIdleConnectionTimeout (t.toMillis.toInt))(Duration.ofMillis(i.getIdleConnectionTimeout )),
      _.getOrUseC[Duration]("pingConnectionInterval" , (c, t) => c.setPingConnectionInterval(t.toMillis.toInt))(Duration.ofMillis(i.getPingConnectionInterval)),
      _.getOrUseC[Duration]("retryInterval"          , (c, t) => c.setRetryInterval         (t.toMillis.toInt))(Duration.ofMillis(40                         )),
      _.getOrUseC[Duration]("timeout"                , (c, t) => c.setTimeout               (t.toMillis.toInt))(Duration.ofMillis(800                        )),
    )
  }

  def config: ConfigDef[RedissonConfig] =
    ( ConfigDef.need[String]("url"),
      extraDef,
    ).mapN(apply)
}
