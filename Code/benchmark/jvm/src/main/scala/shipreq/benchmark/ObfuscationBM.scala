package shipreq.benchmark

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data.ProjectId
import shipreq.webapp.server.logic._

/**
  * > sbt -DMODE=release
  * > benchmark-jvm/jmh:run -wi 10 -i 10 -f 2 ObfuscationBM
  *
  * [info] Benchmark                   Mode  Cnt     Score    Error  Units
  * [info] ObfuscationBM.deobfuscate  thrpt   20  3490.936 ± 50.373  ops/s
  * [info] ObfuscationBM.obfuscate    thrpt   20  3885.695 ± 95.931  ops/s
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class ObfuscationBM {
  import ObfuscationBM._

  @Benchmark def obfuscate = as.map(o.obfuscate)
  @Benchmark def deobfuscate = bs.map(o.deobfuscate)
}

object ObfuscationBM {
  val o = Obfuscators.projectId
  val n = 1000
  val as = (1 to n).map(ProjectId(_))
  val bs = as.map(o.obfuscate)
}