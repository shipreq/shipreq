package shipreq.benchmark

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import shipreq.webapp.base.data._
import shipreq.webapp.sampledata.SampleData

@State(Scope.Benchmark)
@Warmup(iterations = 10, time = 10, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 10, time = 10, timeUnit = TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class ImpFieldCalcBM {

  @Param(Array(
    "real:582",
    "no_req_codes:1000",
    "no_req_codes:2000",
    "no_req_codes:4000",
    "no_req_codes:10000",
  ))
  var sampleData: String = _

  private var p: Project = _
  private var fieldId: CustomField.Implication.Id = _
  private val reqId = GenericReqId(0)

  @Setup
  def setup(): Unit = {
    p = SampleData.byId(sampleData).newProject(deep = false)
    fieldId = p.config.fields.customImpFields.headOption.get.id
  }

  @Benchmark def calculation = p.dataLogic.customFieldImps(HideDead)(fieldId).getPubids(reqId)
}
