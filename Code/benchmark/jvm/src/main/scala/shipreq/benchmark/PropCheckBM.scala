package shipreq.benchmark

import java.util.concurrent.TimeUnit
import nyaya.prop.LogicPropExt
import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data._
import shipreq.webapp.sampledata.SampleData

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class PropCheckBM {

  @Param(Array("full", "no_req_codes"))
  var `type`: String = _

  @Param(Array("1000", "2000", "4000", "10000"))
  var events: String = _

  final private[this] val prop = DataProp.project.allIncludingConfig

  private var p: Project = _

  @Setup
  def setup(): Unit = {
    p = SampleData.byParams(`type`, events).project
  }

  @Benchmark
  def eval = prop(p)
}
