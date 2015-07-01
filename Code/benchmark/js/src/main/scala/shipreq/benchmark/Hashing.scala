package shipreq.benchmark

import shipreq.benchmark.lib.BenchmarkSuite
import shipreq.webapp.base.hash.Hash.HashableValueOps
import shipreq.webapp.base.hash.HashScheme

object Hashing extends BenchmarkSuite("Hashing") {
  implicit val projectHash = HashScheme.default.hashProject
  val p100  = data.project_100
  val p1000 = data.project_1000

  override def configureOptions =
    _.minSamples = 100

  benchmark("hash_100", p100.hash)

  benchmark("hash_1000", p1000.hash)
}