package shipreq.webapp.sampledata

import boopickle.PickleImpl
import io.circe._
import io.circe.syntax._
import shipreq.base.util.BinaryData
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{Event, VerifiedEvent}
import shipreq.webapp.base.protocol.json.v1.Rev1.encoderEvent
import shipreq.webapp.base.protocol.binary.v1.Rev1.{picklerProject, picklerVerifiedEventSeq}

abstract class AbstractSampleData(name: String, events: Vector[Event]) {
  final override def toString: String = s"SampleData($name)"

  lazy val eventJson: Json =
    events.asJson

  lazy val eventJsonStr: String =
    eventJson.noSpacesSortKeys

  lazy val verifiedEvents: VerifiedEvent.Seq =
    SampleDataUtil.verifyEvents(events)

  lazy val verifiedEventsBinary: BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl intoBytes verifiedEvents)

  lazy val project: Project =
    SampleDataUtil.applyVerifiedEventsSuccessfully(Project.empty, verifiedEvents)

  lazy val projectBinary: BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl intoBytes project)
}
