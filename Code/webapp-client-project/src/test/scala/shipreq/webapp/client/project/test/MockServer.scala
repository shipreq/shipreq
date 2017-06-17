package shipreq.webapp.client.project.test

import japgolly.scalajs.react._
import shipreq.base.util.PotentialChange._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.VerifiedEvents
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.base.protocol.RemoteFailure
import shipreq.webapp.client.base.test._
import shipreq.webapp.client.project.app.state.ClientData
import shipreq.webapp.server.logic._
import ProjectSpaProtocols._
import TestClientProtocol.Req

class MockServer(project: CallbackTo[Project], update: VerifiedEvents => Callback) extends TestClientProtocol(true) {

  type Attempt = PartialFunction[(ServerSideProc.Protocol, Any, Project), MakeEvent.Result]

  private def attempt(r: ServerSideProc.Protocol)(f: (r.Input, Project) => MakeEvent.Result): Attempt = {
    case (fn, input, p) if r == fn =>
      f(input.asInstanceOf[r.Input], p)
  }

  private def attemptI(r: ServerSideProc.Protocol)(f: r.Input => MakeEvent.Result): Attempt =
    attempt(r)((i, p) => f(i))

  val handler: Attempt =
    attempt (UpdateContent        )(MakeEvent.updateContent)         orElse
    attempt (CreateContent        )(MakeEvent.createContent)         orElse
    attempt (CustomReqTypeCrud    )(MakeEvent.customReqTypeCrud)     orElse
    attempt (CustomIssueTypeCrud  )(MakeEvent.customIssueTypeCrud)   orElse
    attempt (TagCrud.Protocol     )(MakeEvent.tagCrud)               orElse
    attempt (FieldCrud.Protocol   )(MakeEvent.fieldCrud)             orElse
    attemptI(ReqTypeImplicationMod)(MakeEvent.reqTypeImplicationMod) orElse
    attemptI(FieldMandatorinessMod)(MakeEvent.fieldMandatorinessMod) orElse
    attemptI(ProjectNameSet       )(MakeEvent.projectNameSetFn)

  override def autoResponse(r: Req): Callback =
    project >>= { p1 =>
      // ah the hacks
      def successVE = r.success.asInstanceOf[VerifiedEvents => TCB.Success]

      val h = handler((r.proc.protocol, r.input, p1))
      ApplyNewEvent(h, p1) match {

        case Success(ApplyNewEvent.Updated(p2, ae, ve)) =>
          update(Vector.empty :+ ve) >> successVE(Vector.empty :+ ve).cb

        case Unchanged =>
          Callback.empty >> successVE(Vector.empty).cb

        case Failure(e) =>
          r.failure(RemoteFailure lift e.asInstanceOf[r.proc.protocol.Failure]).cb
      }
    }
}

object MockServer {
  def apply(cd: ClientData): MockServer =
    new MockServer(cd.projectCB, cd.applyEvents)
}
