package shipreq.base.util

import japgolly.microlibs.config._
import scalaz.effect.IO
import scalaz.std.list.listInstance

object Props {

  def fileSources: Sources[IO] =
    Source.propFileOnClasspath[IO]("shipreq.props", optional = true)

  def sources: Sources[IO] =
    Source.environment[IO] >
    fileSources >
    Source.system[IO]
}
