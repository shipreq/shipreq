package shipreq.webapp.sampledata

import java.time.Instant
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event._

private[sampledata] object SampleDataUtil {

  private val startTime = Instant.parse("2020-04-16T00:00:00Z")

  def verifyEvent(e: Event, ord: Int): VerifiedEvent =
    VerifiedEvent(EventOrd(ord), e, startTime.plusSeconds(ord))

  def verifyEvents(es: Vector[Event]): VerifiedEvent.Seq =
    VerifiedEvent.Seq.empty ++ es.indices.iterator.map { i =>
      verifyEvent(es(i), i + 1)
    }

  def applyVerifiedEventSuccessfully(p: Project, e: VerifiedEvent): Project =
    applyVerifiedEventsSuccessfully(p, VerifiedEvent.Seq.empty + e)

  def applyVerifiedEventsSuccessfully(p: Project, es: VerifiedEvent.Seq): Project =
    // Untrusted because usually when I'm running benchmarks I'm modifying the code as well. If I make a small mistake
    // in the code modification I want it to be caught, rather than going unnoticed and reporting spurious results.
    ApplyEvent.untrusted.applyVerified(es)(p).fold(_.throwException(), identity)
}
