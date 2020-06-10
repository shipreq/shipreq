package shipreq.base.ops

import io.jaegertracing.Configuration
import japgolly.clearconfig.internals.ConfigDef

object JaegerTracingConfig {

  def external = ConfigDef.external(
    Configuration.JAEGER_AGENT_HOST,                // The hostname for communicating with agent via UDP
    Configuration.JAEGER_AGENT_PORT,                // The port for communicating with agent via UDP
    Configuration.JAEGER_ENDPOINT,                  // The traces endpoint, in case the client should connect directly to the Collector, like http://jaeger-collector:14268/api/traces
    Configuration.JAEGER_AUTH_TOKEN,                // Authentication Token to send as "Bearer" to the endpoint
    Configuration.JAEGER_USER,                      // Username to send as part of "Basic" authentication to the endpoint
    Configuration.JAEGER_PASSWORD,                  // Password to send as part of "Basic" authentication to the endpoint
    Configuration.JAEGER_PROPAGATION,               // Comma separated list of formats to use for propagating the trace context. Defaults to the standard Jaeger format. Valid values are jaeger and b3
    Configuration.JAEGER_REPORTER_LOG_SPANS,        // Whether the reporter should also log the spans
    Configuration.JAEGER_REPORTER_MAX_QUEUE_SIZE,   // The reporter's maximum queue size
    Configuration.JAEGER_REPORTER_FLUSH_INTERVAL,   // The reporter's flush interval (ms)
    Configuration.JAEGER_SAMPLER_TYPE,              // The sampler type
    Configuration.JAEGER_SAMPLER_PARAM,             // The sampler parameter (number)
    Configuration.JAEGER_SAMPLER_MANAGER_HOST_PORT, // The host name and port when using the remote controlled sampler
    Configuration.JAEGER_TAGS,                      // A comma separated list of name = value tracer level tags, which get added to all reported spans. The value can also refer to an environment variable using the format ${envVarName:default}, where the :default is optional, and identifies a value to be used if the environment variable cannot be found
  )

  def main(serviceName: String): ConfigDef[Option[Configuration]] =
    ConfigDef.getOrUse("jaeger.enabled", false)
      .map(Option.when(_)(Configuration.fromEnv(serviceName)))
}
