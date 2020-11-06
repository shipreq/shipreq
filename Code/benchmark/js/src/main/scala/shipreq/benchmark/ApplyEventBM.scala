package shipreq.benchmark

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.ApplyEvent
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.sampledata.SampleData

object ApplyEventBM {

  private val pe = Project.empty

  sealed abstract class Method(ae: ApplyEvent) {
    def name = toString

    final lazy val bm: Benchmark[SampleData] =
      BenchmarkData.verifiedEvents(name) { events =>
        ae.applyVerified(events)(pe).getOrThrow()
      }
  }

  object Method {
    case object Trusted extends Method(ApplyEvent.trusted)

    // Speed of untrusted doesn't really matter. It only ever does one event at a time.
    // case object Untrusted extends Method(ApplyEvent.untrusted)

    lazy val all      = AdtMacros.adtValues[Method]
    lazy val guiParam = GuiParam.`enum`("Method", all.whole.sortBy(_.name): _*)(_.name)
  }
}

final case class ApplyEventBM(data: BenchmarkData) {
  val suite    = Suite("ApplyEvent")(ApplyEventBM.Method.all.whole.map(_.bm): _*)
  val guiSuite = GuiSuite(suite, data.guiParam)
}
