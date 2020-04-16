package shipreq.benchmark

import java.util.concurrent.TimeUnit
import nyaya.prop.LogicPropExt
import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data.DataProp

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class PropCheck {

  val prop = DataProp.project.allIncludingConfig
  val p  = SampleData.`1000`.project

  @Benchmark
  def eval = prop(p)
}
