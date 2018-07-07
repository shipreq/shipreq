package shipreq.base.util

import japgolly.clearconfig._
import shipreq.base.util.FxModule._

object Props {

  def fileSources: ConfigSources[Fx] =
    ConfigSource.propFileOnClasspath[Fx]("shipreq.properties", optional = true) >
    ConfigSource.propFileOnClasspath[Fx]("db.properties", optional = true) >
    ConfigSource.propFileOnClasspath[Fx]("secret.properties", optional = true)

  def sources: ConfigSources[Fx] =
    ConfigSource.environment[Fx] >
    fileSources >
    ConfigSource.system[Fx]
}