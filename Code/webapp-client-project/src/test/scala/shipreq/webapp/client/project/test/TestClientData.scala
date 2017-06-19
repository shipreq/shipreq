package shipreq.webapp.client.project.test

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.client.project.app.state.{ClientData, ProjectState}

final class TestClientData(initState: ProjectState) extends ClientData(initState) {

  val nextEventOrd: CallbackTo[EventOrd] =
    CallbackTo {
      val s = mutableState.state()
      assert(s.futureEvents.isEmpty)
      s.latestEventOrd + 1
    }

  def verifiedEventSeq(ves: Traversable[VerifiedEvent]): CallbackTo[VerifiedEvent.Seq] =
    NonEmptyVector.option(ves.toVector)
      .map(verifiedEventNES)
      .getOrElse(CallbackTo(VerifiedEvent.EmptySeq))

  def verifiedEventNES(ves: NonEmptyVector[VerifiedEvent]): CallbackTo[VerifiedEvent.NonEmptySeq] =
    nextEventOrd.map(VerifiedEvent.NonEmptySeq(_, ves))

  def applyEventsCB(ves: Traversable[VerifiedEvent]): Callback =
    NonEmptyVector.maybe(ves.toVector, Callback.empty)(
      verifiedEventNES(_).flatMap(applyEventSeqCB))

  def verifyEventsCB(es: Event*): CallbackTo[Vector[VerifiedEvent]] = {
    val v = es.toList // avoid Scala bug
    projectCB.map(verifyEvents(_)(v: _*))
  }

  def applyTestEventsCB(es: Event*): Callback =
    verifyEventsCB(es: _*).flatMap(applyEventsCB)
}

object TestClientData {

  def apply(p: Project): TestClientData =
    new TestClientData(ProjectState.init(
      p, looseProjectMetaData(p, eventCount = 201), EventOrd(200)))
}
