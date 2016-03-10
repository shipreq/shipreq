package shipreq.webapp.client.test

import japgolly.scalajs.react._
import shipreq.webapp.client.app.state.ClientData
import scalaz.\/-
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.VerifiedEvents
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.server._
import shipreq.webapp.client.data.TCB
import shipreq.webapp.client.protocol.ClientProtocol

class MockServer(project: CallbackTo[Project], update: VerifiedEvents => Callback) extends ClientProtocol {

  private def attempt(r: RemoteFn)(f: (r.Input, Project) => MakeEvent.Result): PartialFunction[(RemoteFn, Any, Project), MakeEvent.Result] = {
    case (fn, input, p) if r == fn =>
      f(input.asInstanceOf[r.Input], p)
  }

  private def attemptI(r: RemoteFn)(f: r.Input => MakeEvent.Result) =
    attempt(r)((i, p) => f(i))

  val handler =
    attempt (UpdateContentFn      )(MakeEvent.updateContent)         orElse
    attempt (CreateContentFn      )(MakeEvent.createContent)         orElse
    attempt (CustomReqTypeCrud    )(MakeEvent.customReqTypeCrud)     orElse
    attempt (CustomIssueTypeCrud  )(MakeEvent.customIssueTypeCrud)   orElse
    attempt (TagCrud.Fn           )(MakeEvent.tagCrud)               orElse
    attempt (FieldCrud.Fn         )(MakeEvent.fieldCrud)             orElse
    attemptI(ReqTypeImplicationMod)(MakeEvent.reqTypeImplicationMod) orElse
    attemptI(FieldMandatorinessMod)(MakeEvent.fieldMandatorinessMod)


  override def call(i: RemoteFn.Instance)(input  : i.fn.Input,
                                          success: i.fn.Output => TCB.Success,
                                          failure: ClientProtocol.Failed[i.fn.Failure] => TCB.Failure): Callback =
    project >>= { p1 =>
      val r = handler((i.fn, input, p1))
      ApplyNewEvent(r, p1) match {

        case ApplyNewEvent.Updated(p2, ae, ve) =>
          update(Vector.empty :+ ve)

        case ApplyNewEvent.NoChange =>
          Callback.empty

        case ApplyNewEvent.Failed(e) =>
          failure(\/-(e.asInstanceOf[i.fn.Failure])).cb
      }
    }
}

object MockServer {
  def apply(cd: ClientData): MockServer =
    new MockServer(cd.projectCB, cd.applyEvents)
}
