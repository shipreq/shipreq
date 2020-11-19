package shipreq.webapp.member.test.project

import boopickle.PickleImpl
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.test.JsonTestUtil._
import shipreq.base.util.BinaryData
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.protocol.binary.Latest._
import shipreq.webapp.member.project.protocol.json.Latest._
import shipreq.webapp.member.test.WebappTestUtil._

abstract class RealProject {

  override def toString =
    getClass.getName.stripSuffix("$")

  protected def jsonEvents: Seq[String]

  final val project: Project = {
    val (es, dur1) = time(VerifiedEvent.Seq.empty ++ jsonEvents.iterator.map(decodeOrThrow[VerifiedEvent](_)))
    val (p, dur2) = time(restoreProject(es))
    val dur = dur1.plus(dur2)
    println(s"Materialised $this in ${dur.conciseDesc} (${dur1.conciseDesc} to parse, ${dur2.conciseDesc} to apply)")
    p
  }

  final lazy val projectBinary: BinaryData =
    BinaryData.unsafeFromByteBuffer(PickleImpl.intoBytes(project))
}
