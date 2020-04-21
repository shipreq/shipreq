package shipreq.webapp.sampledata

import java.time.Instant
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._

private[sampledata] object SampleDataUtil {

  private val startTime = Instant.parse("2020-04-16T00:00:00Z")

  def verifyEvent(e: Event, ord: Int): VerifiedEvent =
    VerifiedEvent(EventOrd(ord), e, startTime.plusSeconds(ord))

  def verifyEvents(es: Vector[Event]): VerifiedEvent.Seq =
    VerifiedEvent.Seq.empty ++ es.indices.iterator.map { i =>
      verifyEvent(es(i), i + 1)
    }

  def applyVerifiedEventSuccessfully(p: Project, e: VerifiedEvent): Project =
    ApplyEvent.untrusted.applyVerified(Vector(e))(p).fold(sys.error, identity)

  def applyVerifiedEventsSuccessfully(p: Project, es: VerifiedEvent.Seq): Project =
    es.foldLeft(p)(applyVerifiedEventSuccessfully)
}
