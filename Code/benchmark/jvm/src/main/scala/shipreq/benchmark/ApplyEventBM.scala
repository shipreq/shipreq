package shipreq.benchmark

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scalaz.{-\/, \/-}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.sampledata.SampleData

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class ApplyEventBM {

  @Param(Array("1000", "2000", "4000", "10000"))
  var events: String = _

  var es: VerifiedEvent.Seq = _

  val pe           = Project.empty
  val ae_trusted   = ApplyEvent.trusted
  val ae_untrusted = ApplyEvent.untrusted

  @Setup
  def setup(): Unit = {
    es = SampleData.byName(events).verifiedEvents
  }

  private def go(ae: ApplyEvent): Project =
    ae.applyVerified(es)(pe) match {
      case \/-(p) => p
      case -\/(e) => println(e); sys.error(e)
    }

  // Speed of untrusted doesn't really matter. It only ever does one event at a time.
  // @Benchmark def untrusted = go(ae_untrusted)

  @Benchmark def trusted = go(ae_trusted)
}