package shipreq.benchmark

/* > benchmark-jvm/jmh:run EventBM
[info] Benchmark          Mode  Cnt  Score   Error  Units
[info] EventBM.trusted    avgt   12  4.659 ± 0.227  ms/op
[info] EventBM.untrusted  avgt   12  4.639 ± 0.208  ms/op
 */

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import scalaz.{-\/, \/-}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.ApplyEvent

@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class EventBM {

  val events       = SampleData.project3Events
  val pe           = Project.empty
  val ae_trusted   = ApplyEvent.trusted
  val ae_untrusted = ApplyEvent.untrusted

  def go(ae: ApplyEvent): Project =
    ae_untrusted.applyVerified(events)(pe) match {
      case \/-(p) => p
      case -\/(e) => println(e); sys.error(e)
    }

  @Benchmark def untrusted = go(ae_untrusted)
  @Benchmark def trusted   = go(ae_trusted)
}