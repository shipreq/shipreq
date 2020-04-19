package shipreq.benchmark

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.ApplyEvent
import shipreq.webapp.base.test.WebappTestUtil._

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
    case object Untrusted extends Method(ApplyEvent.untrusted)

    val all      = AdtMacros.adtValues[Method]
    val guiParam = GuiParam.`enum`("Method", all.whole.sortBy(_.name): _*)(_.name, initialValues = List(Trusted))
  }
}

final case class ApplyEventBM(data: BenchmarkData) {
  val suite    = Suite("ApplyEvent")(ApplyEventBM.Method.all.whole.map(_.bm): _*)
  val guiSuite = GuiSuite(suite, data.guiParam)
}
