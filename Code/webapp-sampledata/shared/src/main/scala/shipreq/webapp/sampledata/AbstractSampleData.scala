package shipreq.webapp.sampledata

import boopickle.PickleImpl
import io.circe._
import io.circe.syntax._
import shipreq.base.util.BinaryData
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{Event, VerifiedEvent}
import shipreq.webapp.member.project.protocol.binary.v1.Latest.{picklerProject, picklerVerifiedEventSeq}
import shipreq.webapp.member.project.protocol.json.v1.Latest.encoderEvent

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

  def newProject(deep: Boolean = true): Project =
    if (deep)
      meta.annotateExceptions(SampleDataUtil.applyVerifiedEventsSuccessfully(Project.empty, verifiedEvents))
    else
      project.copy()

  lazy val project: Project =
    newProject()

  lazy val projectBinary: BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl intoBytes project)

  lazy val errors: List[String] =
    assertHashcode("project.config", project.config, meta.projectConfigHash) ++
    assertHashcode("project.content", project.content, meta.projectContentHash)

  private def assertHashcode(name: String, a: Any, expected: Int): List[String] =
    if (a.hashCode == expected)
      Nil
    else
      s"${meta.filename} $name hashcode mismatch! (a) ${a.hashCode} =!= $expected (e)" :: Nil

  def assertValid(): Unit =
    if (errors.nonEmpty)
      throw new AssertionError(errors.mkString("\n"))
}
