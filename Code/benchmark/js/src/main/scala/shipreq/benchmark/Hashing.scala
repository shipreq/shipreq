package shipreq.benchmark

import japgolly.scalajs.benchmark._, gui._
import shipreq.webapp.base.hash.Hash.HashableValueOps
import shipreq.webapp.base.hash.HashScheme

object Hashing {
  implicit val projectHash = HashScheme.latest.value.hashProject

  val suite = GuiSuite(
    Suite("Hashing")(
      projectBM("Hash project")(_.hash)
    )
  )
}