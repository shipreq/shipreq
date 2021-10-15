package shipreq.base.util

import japgolly.clearconfig._
import japgolly.clearconfig.internals.Key
import shipreq.base.util.FxModule._

object Props {

  def fileSources: ConfigSources[Fx] =
    ConfigSource.propFileOnClasspath[Fx]("shipreq.properties", optional = true) >
    ConfigSource.propFileOnClasspath[Fx]("secret.properties", optional = true)

  def sources: ConfigSources[Fx] =
    ConfigSource.environment[Fx].expandInlineProperties("SHIPREQ_INLINE_PROPERTIES").mapKeyQueries(acceptExternalKeyFormat) >
    fileSources >
    ConfigSource.system[Fx].mapKeyQueries(acceptExternalKeyFormat)

  private def acceptExternalKeyFormat(k: Key): List[Key] =
    k :: k.map(_.toUpperCase.replace('.', '_')) :: Nil
}