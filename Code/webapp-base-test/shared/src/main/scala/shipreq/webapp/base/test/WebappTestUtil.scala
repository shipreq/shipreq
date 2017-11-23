package shipreq.webapp.base.test

import java.time.Instant
import java.time.temporal.ChronoUnit._
import scalaz.{-\/, \/-}
import shipreq.base.test._
import shipreq.webapp.base.event._
import shipreq.webapp.base.data._
import shipreq.webapp.base.hash.HashSchemes
import shipreq.webapp.base.text.Text

trait WebappTestEquality
  extends BaseTestEquality
     with Text.Equality
     with EventEquality
{
  implicit def equalityText = ReqData.equalityText
  implicit def equalityTags = ReqData.equalityTags
}

object WebappTestUtil extends WebappTestEquality with WebappTestUtil

trait WebappTestUtil extends BaseTestUtil {

  def looseProjectMetaData(p: Project, eventCount: Int = 123): ProjectMetaData =
    ProjectMetaData(
      Obfuscated("t3sT"),
      p.name,
      eventCount,
      p.content.reqs.size,
      Instant.now().minus(28, DAYS),
      Some(Instant.now().minus(1, DAYS)))

  def verifyEvent(p: Project, e: Event, o: EventOrd = EventOrd(-1)): VerifiedEvent =
    _verifyEvent(p, e, o)._2

  def _verifyEvent(p: Project, e: Event, o: EventOrd = EventOrd(-1)): (Project, VerifiedEvent) = {
    val p2 = ApplyEvent.untrusted.apply1(e)(p).fold(err => sys error s"Failed to apply event $e: $err", identity)
    val hrs = HashSchemes.latest.changes(p, p2)
    (p2, VerifiedEvent(o, e, hrs))
  }

  def verifyEvents(p0: Project, firstOrd: EventOrd = EventOrd(1))(es: Event*): VerifiedEvent.Seq = {
    var p = p0
    VerifiedEvent.Seq.empty ++ es.iterator.zipWithIndex.map { case (e, i) =>
      val (p2, ve) = _verifyEvent(p, e, firstOrd + i)
      p = p2
      ve
    }
  }

  def applyEventSuccessfully(p: Project, e: Event): Project =
    _verifyEvent(p, e)._1

  def applyEventsSuccessfully(p: Project, es: Event*): Project =
    es.foldLeft(p)(applyEventSuccessfully)

  def applyVerifiedEventSuccessfully(p: Project, e: VerifiedEvent): Project =
    ApplyEvent.untrusted.applyVerified(Vector(e))(p).fold(sys.error, identity)

  def applyVerifiedEventSuccessfully(p: Project, es: VerifiedEvent*): Project =
    es.foldLeft(p)(applyVerifiedEventSuccessfully)

  def assertEventFails(p: Project, e: Event, errFrag: String = ""): Unit =
    ApplyEvent.untrusted.apply1(e)(p) match {
      case -\/(f) => assertContainsCI(f, errFrag)
      case \/-(_) => fail(s"Failure expected but didn't occur applying $e")
    }

  implicit final class VerifiedEventSeqExt(private val self: VerifiedEvent.Seq) {
    def needNES: VerifiedEvent.NonEmptySeq =
      VerifiedEvent.NonEmptySeq(self.head, self.tail)
  }

}