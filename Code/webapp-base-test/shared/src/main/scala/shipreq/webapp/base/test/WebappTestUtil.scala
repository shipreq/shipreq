package shipreq.webapp.base.test

import shipreq.base.test._
import shipreq.webapp.base.event._
import shipreq.webapp.base.data._
import shipreq.webapp.base.hash.HashRec
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

  def verifyEvent(p: Project, e: Event): VerifiedEvent =
    _verifyEvent(p, e)._2

  def _verifyEvent(p: Project, e: Event): (Project, VerifiedEvent) = {
    val p2 = ApplyEvent.untrusted.apply1(e)(p).fold(sys.error, identity)
    val hrs = HashRec.changes(p, p2)
    (p2, VerifiedEvent(e, hrs))
  }

  def verifyEvents(p0: Project)(es: Event*): VerifiedEvents = {
    var p = p0
    es.toVector.map { e =>
      val (p2, ve) = _verifyEvent(p, e)
      p = p2
      ve
    }
  }

  def applyEventSuccessfully(p: Project, e: Event): Project =
    _verifyEvent(p, e)._1

  def applyEventsSuccessfully(p: Project, es: Event*): Project =
    es.foldLeft(p)(applyEventSuccessfully)

  def applyVerifiedEventSuccessfully(p: Project, e: VerifiedEvent): Project =
    ApplyEvent.untrusted.applyVerified(e :: Nil)(p).fold(sys.error, identity)

  def applyVerifiedEventSuccessfully(p: Project, es: VerifiedEvent*): Project =
    es.foldLeft(p)(applyVerifiedEventSuccessfully)
}