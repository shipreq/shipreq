package shipreq.webapp.client.project.test

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

  def verifyEventsCB(es: Event*): CallbackTo[VerifiedEvent.Seq] = {
    val eventList = es.toList // avoid Scala bug
    projectCB.flatMap(p =>
      CallbackTo.liftTraverse((e: Event) => nextEventOrd.map(verifyEvent(p, e, _))).std[List]
        .map(VerifiedEvent.Seq.empty ++ _ (eventList)))
  }

  def applyTestEventsCB(es: Event*): Callback =
    verifyEventsCB(es: _*).flatMap(applyEventSeqCB)
}

object TestClientData {

  def apply(p: Project): TestClientData =
    new TestClientData(ProjectState.init(
      p, looseProjectMetaData(p, totalEventCount = 201), EventOrd(200)))
}
