package shipreq.benchmark

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.data.derivation.AtomScan
import shipreq.webapp.sampledata.SampleData

/*

> jmh:run -wi 1 -i 3 -f 1 AtomScanBM

[info] AtomScanBM.atomScan     10000  avgt    3  2.333 ± 0.462  ms/op

 */

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class AtomScanBM {

//  @Param(Array("1000", "2000", "4000", "10000"))
  @Param(Array("10000"))
  var events: String = _

  var p: Project = _

  @Setup
  def setup(): Unit = {
    p = SampleData.byName(events).project
  }

  @Benchmark def atomScan = AtomScan(p)
}
