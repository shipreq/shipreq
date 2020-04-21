package shipreq.webapp.base.test

import io.circe.parser._
import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.univeq.UnivEqScalaz._
import java.time.Instant
import java.time.temporal.ChronoUnit._
import scalaz.{-\/, Equal, \/-}
import sourcecode.Line
import shipreq.base.test._
import shipreq.webapp.base.event._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter._
import shipreq.webapp.base.protocol.json.v1.Rev1._
import shipreq.webapp.base.text.Text

trait WebappTestEquality
  extends BaseTestEquality
     with Text.Equality
     with EventEquality
{
  implicit def equalityText = ReqData.equalityText
  implicit def equalityTags = ReqData.equalityTags

  implicit lazy val equalProjectAndOrd: Equal[ProjectAndOrd] = ScalazMacros.deriveEqual
}

object WebappTestUtil extends WebappTestEquality with WebappTestUtil

trait WebappTestUtil extends BaseTestUtil {

  def looseProjectMetaData(p: Project, eventsTotal: Int = 123, eventsInit: Int = 2): ProjectMetaData =
    ProjectMetaData.fromProject(p)(
      id            = Obfuscated("t3sT"),
      eventsInit    = eventsInit.min(eventsTotal),
      eventsTotal   = eventsTotal,
      createdAt     = Instant.now().minus(28, DAYS),
      accessedAt    = Instant.now().minus(1, DAYS),
      lastUpdatedAt = Some(Instant.now().minus(1, DAYS)))

  def verifyEvent(p: Project, e: Event, o: EventOrd = EventOrd.first): VerifiedEvent =
    _verifyEvent(p, e, o)._2

  def _verifyEvent(p: Project, e: Event, o: EventOrd = EventOrd.first): (Project, VerifiedEvent) = {
    val p2 = ApplyEvent.untrusted.apply1(e)(p).fold(err => sys error s"Failed to apply event $e: $err", identity)
    (p2, VerifiedEvent(o, e, Instant.now()))
  }

  def verifyEvents(p0: Project, firstOrd: EventOrd = EventOrd.first)(es: Event*): VerifiedEvent.Seq = {
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

  def applyVerifiedEventsSuccessfully(p: Project, es: VerifiedEvent.Seq): Project =
    es.foldLeft(p)(applyVerifiedEventSuccessfully)

  def assertEventFails(p: Project, e: Event, errFrag: String = "")(implicit l: Line): Unit =
    ApplyEvent.untrusted.apply1(e)(p) match {
      case -\/(f) => assertContainsCI(f, errFrag)
      case \/-(_) => fail(s"Failure expected but didn't occur applying $e")
    }

  implicit final class WebappTestUtilExt_VerifiedEventSeq(private val self: VerifiedEvent.Seq) {
    def needNES: VerifiedEvent.NonEmptySeq =
      VerifiedEvent.NonEmptySeq(self.head, self.tail)
  }

  implicit final class WebappTestUtilExt_Event(private val self: Event) {
    def active: ActiveEvent =
      self match {
        case a: ActiveEvent => a
        case r: RetiredEvent => sys.error(s"Not an active event: $r")
      }
  }

//  implicit def autoSeqIssueToSeqIssueLite(is: Seq[Issue]): Seq[IssueLite] =
//    is.map(IssueLite.fromIssue)

  def assertIssueSet(actual: Seq[IssueLite], expect: Seq[IssueLite])(implicit l: Line): Unit =
    assertIssueSet("", actual, expect)

  def assertIssueSet(name: => String, actual: Seq[IssueLite], expect: Seq[IssueLite])(implicit l: Line): Unit = {
    assertSeqIgnoreOrder(name, actual, expect)
    def norm(i: Seq[IssueLite]) = MutableArray(i).sortBySchwartzian(_.toString).iterator.to(Vector)
    assertSeq(name, norm(actual), norm(expect))
  }

  def verifiedEventsFromJson(jsons: String*): VerifiedEvent.Seq =
    VerifiedEvent.Seq.empty ++ jsons.iterator.map(decode[VerifiedEvent](_).getOrThrow())

  def parseFilterSuccessfully(cfg: ProjectConfig): String => Filter.Valid = {
    val validator = FilterAlgebra.validate(cfg)
    filterTxt => {
      val pf        = FilterParser.parse(filterTxt).getOrThrow().get
      Filter.Potential.validate(pf, validator).getOrThrow()
    }
  }
}