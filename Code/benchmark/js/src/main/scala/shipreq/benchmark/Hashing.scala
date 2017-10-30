package shipreq.benchmark

import japgolly.scalajs.benchmark._, gui._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.hash._

object Hashing {
  val projectHash: Project => Int = HashScheme.latest.hasher(HashScope.WholeProject, _)

  val suite = GuiSuite(
    Suite("Hashing")(
      projectBM("Hash project")(projectHash)
    )
  )
}