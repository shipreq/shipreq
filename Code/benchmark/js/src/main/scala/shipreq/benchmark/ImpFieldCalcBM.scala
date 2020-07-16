package shipreq.benchmark

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui.GuiSuite
import shipreq.webapp.base.data.{GenericReqId, HideDead}
import shipreq.webapp.sampledata.SampleData

final case class ImpFieldCalcBM(data: BenchmarkData) {
  private val reqId = GenericReqId(0)

  val bm: Benchmark[SampleData] =
    BenchmarkData
      .newProject(deep = false)
      .map(p => (p, p.config.fields.customImpFields.headOption.get.id))
      .apply("Calculation") { case (p, fieldId) => p.dataLogic.customFieldImps(HideDead)(fieldId).getPubids(reqId) }

  val suite    = Suite("ImpField calculation")(bm)
  val guiSuite = GuiSuite(suite, data.guiParam)
}
