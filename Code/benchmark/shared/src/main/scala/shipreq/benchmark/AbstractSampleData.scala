package shipreq.benchmark

import boopickle.PickleImpl
import java.time.Instant
import shipreq.base.util.BinaryData
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{Event, EventOrd, VerifiedEvent}
import shipreq.webapp.base.protocol.binary.v1.Rev1.{picklerProject, picklerVerifiedEventSeq}
import shipreq.webapp.base.test.WebappTestUtil._

abstract class AbstractSampleData(name: String, events: Vector[Event]) {
  final override def toString: String = s"SampleData($name)"

  private val startTime = Instant.parse("2020-04-16T00:00:00Z")

  private def verifyEvents(es: Vector[Event]): VerifiedEvent.Seq =
    VerifiedEvent.Seq.empty ++ es.indices.iterator.map { i =>
      val e = es(i)
      VerifiedEvent(EventOrd(i), e, startTime.plusSeconds(i))
    }

  lazy val verifiedEvents: VerifiedEvent.Seq =
    verifyEvents(events)

  lazy val verifiedEventsBinary: BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl intoBytes verifiedEvents)

  lazy val project: Project =
    applyEventsSuccessfully(Project.empty, events: _*)

  lazy val projectBinary: BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl intoBytes project)
}
