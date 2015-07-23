package shipreq.benchmark

import japgolly.nyaya.LogicPropExt
import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data.DataProp

@State(Scope.Benchmark)
class PropCheck {

  val prop = DataProp.project.allIncludingConfig
  val p100  = data.project_100

  @Benchmark
  def prop_100 = prop(p100)
}
