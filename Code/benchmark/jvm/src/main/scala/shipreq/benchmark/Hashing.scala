package shipreq.benchmark

import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.hash.{HashScheme, HashScope}

@State(Scope.Benchmark)
class Hashing {

  val projectHash: Project => Int = HashScheme.latest.hasher(HashScope.WholeProject, _)
  val p100  = data.project_100
//  val p1000 = data.project_1000

  @Benchmark
  def hash_100 = projectHash(p100)

//  @Benchmark
//  def hash_1000 = projectHash(p1000)
}
