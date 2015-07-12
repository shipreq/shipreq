package shipreq.benchmark

import org.openjdk.jmh.annotations._
import shipreq.webapp.base.hash.Hash.HashableValueOps
import shipreq.webapp.base.hash.HashScheme

@State(Scope.Benchmark)
class Hashing {

  implicit val projectHash = HashScheme.default.hashProject
  val p100  = data.project_100
//  val p1000 = data.project_1000

  @Benchmark
  def hash_100 = p100.hash

//  @Benchmark
//  def hash_1000 = p1000.hash
}
