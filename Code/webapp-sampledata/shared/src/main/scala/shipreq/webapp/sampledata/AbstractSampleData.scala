package shipreq.webapp.sampledata

import boopickle.PickleImpl
import io.circe._
import io.circe.syntax._
import shipreq.base.util.BinaryData
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{Event, VerifiedEvent}
import shipreq.webapp.base.protocol.json.v1.Rev1.encoderEvent
import shipreq.webapp.base.protocol.binary.v1.Rev1.{picklerProject, picklerVerifiedEventSeq}

abstract class AbstractSampleData(meta: SampleDataMeta, events: Vector[Event]) {
  final override def toString: String = s"SampleData($name)"

  def name: String =
    meta.filename

  lazy val eventJson: Json =
    events.asJson

  lazy val eventJsonStr: String =
    eventJson.noSpacesSortKeys

  lazy val verifiedEvents: VerifiedEvent.Seq =
    SampleDataUtil.verifyEvents(events)

  lazy val verifiedEventsBinary: BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl intoBytes verifiedEvents)

  lazy val project: Project = {
    val p = SampleDataUtil.applyVerifiedEventsSuccessfully(Project.empty, verifiedEvents)
    p
  }

  lazy val projectBinary: BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl intoBytes project)

  lazy val errors: List[String] =
    assertHashcode("project.config", project.config, meta.projectConfigHash) ++
    assertHashcode("project.content", project.content, meta.projectContentHash)

  private def assertHashcode(name: String, a: Any, expected: Int): List[String] =
    if (a.hashCode == expected)
      Nil
    else
      s"${meta.filename} $name hashcode mismatch! (a) ${a.hashCode} ≠ $expected (e)" :: Nil

  def assertValid(): Unit =
    if (errors.nonEmpty)
      throw new AssertionError(errors.mkString("\n"))
}
