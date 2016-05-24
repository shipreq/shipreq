package shipreq.webapp.client.project.test

import japgolly.scalajs.react._
import scalaz.\/-
import shipreq.base.util.ValidUpdate._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.VerifiedEvents
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.server._
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.project.app.state.ClientData
import TestClientProtocol.Req

class MockServer(project: CallbackTo[Project], update: VerifiedEvents => Callback) extends TestClientProtocol {

  type Attempt = PartialFunction[(RemoteFn, Any, Project), MakeEvent.Result]

  private def attempt(r: RemoteFn)(f: (r.Input, Project) => MakeEvent.Result): Attempt = {
    case (fn, input, p) if r == fn =>
      f(input.asInstanceOf[r.Input], p)
  }

  private def attemptI(r: RemoteFn)(f: r.Input => MakeEvent.Result): Attempt =
    attempt(r)((i, p) => f(i))

  val handler: Attempt =
    attempt (UpdateContentFn      )(MakeEvent.updateContent)         orElse
    attempt (CreateContentFn      )(MakeEvent.createContent)         orElse
    attempt (CustomReqTypeCrud    )(MakeEvent.customReqTypeCrud)     orElse
    attempt (CustomIssueTypeCrud  )(MakeEvent.customIssueTypeCrud)   orElse
    attempt (TagCrud.Fn           )(MakeEvent.tagCrud)               orElse
    attempt (FieldCrud.Fn         )(MakeEvent.fieldCrud)             orElse
    attemptI(ReqTypeImplicationMod)(MakeEvent.reqTypeImplicationMod) orElse
    attemptI(FieldMandatorinessMod)(MakeEvent.fieldMandatorinessMod)


  override def autoResponse(r: Req): Callback =
    project >>= { p1 =>
      // ah the hacks
      def successVE = r.success.asInstanceOf[VerifiedEvents => TCB.Success]

      val h = handler((r.r.fn, r.input, p1))
      ApplyNewEvent(h, p1) match {

        case Success(ApplyNewEvent.Updated(p2, ae, ve)) =>
          update(Vector.empty :+ ve) >> successVE(Vector.empty :+ ve).cb

        case Unchanged =>
          Callback.empty >> successVE(Vector.empty).cb

        case Failure(e) =>
          r.failure(\/-(e.asInstanceOf[r.r.fn.Failure])).cb
      }
    }
}

object MockServer {
  def apply(cd: ClientData): MockServer =
    new MockServer(cd.projectCB, cd.applyEvents)
}
