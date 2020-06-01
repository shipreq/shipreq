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

  @Param(Array("full", "no_req_codes"))
  var `type`: String = _

  @Param(Array("1000", "2000", "4000", "10000"))
  var events: String = _

  var es: VerifiedEvent.Seq = _

  val pe         = Project.empty
  val ae_trusted = ApplyEvent.trusted

  @Setup
  def setup(): Unit = {
    es = SampleData.byParams(`type`, events).verifiedEvents
  }

  private def go(ae: ApplyEvent): Project =
    ae.applyVerified(es)(pe) match {
      case \/-(p) => p
      case -\/(e) => println(e); e.throwException()
    }

  // Speed of untrusted doesn't really matter. It only ever does one event at a time.
  // @Benchmark def untrusted = go(ae_untrusted)

  @Benchmark def trusted = go(ae_trusted)
}