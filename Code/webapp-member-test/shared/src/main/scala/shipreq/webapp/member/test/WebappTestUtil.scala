package shipreq.webapp.member.test

import cats.Eq
import io.circe.parser._
import japgolly.microlibs.cats_ext.CatsMacros
import japgolly.microlibs.stdlib_ext.MutableArray
import java.time.Instant
import java.time.temporal.ChronoUnit._
import shipreq.base.test._
import shipreq.webapp.base.util._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.filter._
import shipreq.webapp.member.project.protocol.json.Latest._
import shipreq.webapp.member.project.text.Text
import shipreq.webapp.member.test.project.{EventEquality, IssueLite}
import sourcecode.Line

trait WebappTestEquality
  extends BaseTestEquality
     with Text.Equality
     with EventEquality
{
  implicit def equalityTags = ReqData.equalityTags
}

object WebappTestUtil extends WebappTestEquality with WebappTestUtil {

  @nowarn("cat=unused")
  def projectEqualityWithHistoryTimestampEquality(implicit equalInstant: Equal[Instant]): Project.Equality = {

    implicit val equalVerifiedEvent: Equal[VerifiedEvent] =
      ScalazMacros.deriveEqual

    implicit val equalVerifiedEventSeq: Equal[VerifiedEvent.Seq] =
      Equal.equalBy(_.toList)

    implicit val equalProjectEvents: Equal[ProjectEvents] =
      ScalazMacros.deriveEqual

    new Project.Equality
  }

  lazy val ImplicitProjectEqualityDeep =
    projectEqualityWithHistoryTimestampEquality(implicitly)

  lazy val ImplicitProjectEqualityDeepExceptEventTime =
    projectEqualityWithHistoryTimestampEquality((_, _) => true)

}

trait WebappTestUtil extends BaseTestUtil {

  def looseProjectMetaData(p: Project, eventsTotal: Int = 123, eventsInit: Int = 2): ProjectMetaData =
    ProjectMetaData.fromProject(p)(
      id            = Obfuscated("t3sT"),
      eventsInit    = eventsInit.min(eventsTotal),
      eventsTotal   = eventsTotal,
      createdAt     = Instant.now().minus(28, DAYS),
      accessedAt    = Instant.now().minus(1, DAYS),
      lastUpdatedAt = Some(Instant.now().minus(1, DAYS)))

  def verifyEvent(p: Project, e: Event): VerifiedEvent = {
    val ve = VerifiedEvent(p.history.nextOrd, e, Instant.now())
    applyVerifiedEventSuccessfully(p, ve)
    ve
  }

  def verifyEvents(p0: Project)(es: Event*): VerifiedEvent.Seq = {
    var p = p0
    VerifiedEvent.Seq.empty ++ es.iterator.map { e =>
      val ve = VerifiedEvent(p.history.nextOrd, e, Instant.now())
      p = applyVerifiedEventSuccessfully(p, ve)
      ve
    }
  }

  def applyEventSuccessfully(p: Project, e: Event): Project = {
    val ve = VerifiedEvent(p.history.nextOrd, e, Instant.now())
    applyVerifiedEventSuccessfully(p, ve)
  }

  def applyEventsSuccessfully(p: Project, es: Event*): Project =
    es.foldLeft(p)(applyEventSuccessfully)

  def applyVerifiedEventSuccessfully(p: Project, e: VerifiedEvent): Project =
    ApplyEvent.untrusted(e)(p).fold(_.throwException(), identity)

  def applyVerifiedEventSuccessfully(p: Project, es: VerifiedEvent*): Project =
    es.foldLeft(p)(applyVerifiedEventSuccessfully)

  def applyVerifiedEventsSuccessfully(p: Project, es: VerifiedEvent.Seq): Project =
    es.foldLeft(p)(applyVerifiedEventSuccessfully)

  def assertEventFails(p: Project, e: Event, errFrag: String = "")(implicit l: Line): Unit =
    ApplyEvent.untrusted.partialApplyUnverified(e)(p) match {
      case -\/(f) => assertContainsCI(f.value, errFrag)
      case \/-(_) => fail(s"Failure expected but didn't occur applying $e")
    }

  def restoreProject(es: VerifiedEvent.Seq): Project =
    Project.empty.updateOrThrow(es)

  def setOrd(p: Project, ord: EventOrd): Project = {
    val now = Instant.now().minusSeconds(ord.value + 1)
    def ve(o: EventOrd) = VerifiedEvent(o, Event.ProjectNameSet(o.value.toString), now.plusSeconds(o.value))
    var events = p.history.events
    if (events.nonEmpty)
      events = events.takeWhile(_.ord <= ord)
    if (events.isEmpty)
      events += ve(EventOrd.first)
    var last = events.last
    while (last.ord < ord) {
      last = ve(last.ord + 1)
      events += last
    }
//    println()
//    println(s"setOrd($p, $ord):")
//    events.foreach(e => println("  " + e))
//    println()
    p.copy(history = ProjectEvents(events))
  }

  def newProject(ord: Int): Project =
    if (ord == 0)
      Project.empty
    else
      setOrd(Project.empty, EventOrd(ord))

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
    def norm(i: Seq[IssueLite]) = MutableArray(i).sortBySchwartzian(_.toString).iterator().to(Vector)
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