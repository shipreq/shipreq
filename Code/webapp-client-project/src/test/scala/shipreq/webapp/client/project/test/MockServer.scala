package shipreq.webapp.client.project.test

import japgolly.microlibs.nonempty.NonEmptyVector
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.ErrorMsg
import shipreq.base.util.PotentialChange._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.test._
import shipreq.webapp.server.logic._
import ProjectSpaProtocols._

final case class MockServer(cd: TestClientData) extends TestClientProtocol(true) {

  type Attempt = PartialFunction[(ServerSideProc.Protocol[_, _], Any, Project), MakeEvent.Result]

  private def attempt[I, O](r: ServerSideProc.Protocol[I, O])(f: (I, Project) => MakeEvent.Result): Attempt = {
    case (fn, input, p) if r == fn =>
      f(input.asInstanceOf[r.Input], p)
  }

  private def attemptI[I](r: ServerSideProc.Protocol[I, ErrorMsg \/ VerifiedEvent.Seq])(f: I => MakeEvent.Result): Attempt =
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

  autoResponseFallback = r =>
    cd.projectCB >>= { p1 =>
      // ah the hacks
      def onResponse = r.forceIO[Nothing, (ErrorMsg \/ VerifiedEvent.Seq)].onResponse

      val h = handler((r.proc.protocol, r.input, p1))
      ApplyNewEvent(h, p1) match {

        case Success(ApplyNewEvent.Updated(p2, event, hrs)) =>
          cd.nextEventOrd
            .map(o => VerifiedEvent.Seq.empty + VerifiedEvent(o, event, hrs))
            .flatTap(cd.applyEventSeqCB)
            .flatMap(ves => onResponse(\/-(\/-(ves))))

        case Unchanged =>
          onResponse(\/-(\/-(VerifiedEvent.Seq.empty)))

        case Failure(e) =>
          onResponse(\/-(-\/(ErrorMsg(e))))
      }
    }
}
