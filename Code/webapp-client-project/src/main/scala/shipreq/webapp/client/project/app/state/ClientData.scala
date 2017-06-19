package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react.extra.{Broadcaster, Px, Reusability}
import japgolly.scalajs.react.{Callback, CallbackTo}
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.{ErrorMsg, ProjectSpaProtocols, ServerSideProc}
import shipreq.webapp.client.base.data.TCB
import shipreq.webapp.client.base.protocol.{ClientProtocol, ServerSideProcInvoker}

class ClientData(initialState: ProjectState) extends Broadcaster[Changes] {

  protected val mutableState = new ProjectState.Mutable(initialState)

  // Broadcast changes
  mutableState.addListener((ves, ps1, ps2) =>
    ves match {
      case ne: VerifiedEvent.NonEmptySeq => broadcast(Changes(ne, ps1.project, ps2.project))
      case VerifiedEvent.EmptySeq        => Callback.empty // Events queued but not applied
    })

  // Old API
  final def project()                               : Project             = mutableState.pxProject.value()
  final def projectMetaData()                       : ProjectMetaData     = mutableState.state().projectMetaData
  final val pxProject                               : Px[Project]         = mutableState.pxProject
  final val projectCB                               : CallbackTo[Project] = pxProject.toCallback
  final def applyEventSeqCB(ves: VerifiedEvent.Seq) : Callback            = mutableState.applyEventSeqCB(ves)
  final def applyEventSeqSCB(ves: VerifiedEvent.Seq): TCB.Success         = mutableState.applyEventSeqSCB(ves)

  final def serverSideProcToEvents[I](proc: ServerSideProc.Aux[ErrorMsg, I, VerifiedEvent.Seq],
                                      cp: ClientProtocol): ServerSideProcInvoker[I, VerifiedEvent.Seq] =
    new ServerSideProcInvoker((i, s, f) =>
      cp.call(proc)(i, e => mutableState.applyEventSeqSCB(e) >> s(e), _ consumeAnd f))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object ClientData {

  @inline implicit def reusability = Reusability.byRef[ClientData]

  def initAsync(initMetaData: ProjectMetaData,
                cp          : ClientProtocol,
                remoteInit  : ProjectSpaProtocols.InitAsync.Instance)
               (onSuccess   : ClientData => Callback,
                onFailure   : String => Callback): Callback =
    cp.call(remoteInit)((),
      i => {
        val s = ProjectState.init(i.project, initMetaData, i.latestEventOrd)
        def cd = new ClientData(s)
        TCB.Success(onSuccess(cd))
      },
      _.consumeAnd(e => TCB.Failure(onFailure(e))))
}