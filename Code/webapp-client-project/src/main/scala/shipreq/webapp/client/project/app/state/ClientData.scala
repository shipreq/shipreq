package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react.extra.{Broadcaster, Px}
import japgolly.scalajs.react.{Callback, CallbackTo, Reusability}
import scalaz.\/
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.{ProjectSpaProtocols, ServerSideProcInvoker}

class ClientData(initialState: ProjectState) extends Broadcaster[Changes] {

  protected val mutableState = new ProjectState.Mutable(initialState)

  // Broadcast changes
  mutableState.addListener(c =>
    broadcast(Changes(c.events, c.oldState.project, c.newState.project)))

  // Old API
  final def project()                               : Project             = mutableState.pxProject.value()
  final def projectMetaData()                       : ProjectMetaData     = mutableState.state().projectMetaData
  final val pxProject                               : Px[Project]         = mutableState.pxProject
  final val projectCB                               : CallbackTo[Project] = pxProject.toCallback
  final def applyEventSeqCB(ves: VerifiedEvent.Seq) : Callback            = mutableState.applyEventSeqCB(ves)

//  final def serverSideProcToEvents[I](proc: ServerSideProc[I, ErrorMsg \/ VerifiedEvent.Seq]): ServerSideProcInvoker[I, ErrorMsg, VerifiedEvent.Seq] =
//    cp(proc)
//      .mergeFailure
//      .onSuccess((ves, s) => s << mutableState.applyEventSeqCB(ves))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object ClientData {

  @inline implicit def reusability = Reusability.byRef[ClientData]

//  def initAsync(cp          : ClientProtocol,
//                initAppData : ProjectSpaProtocols.InitAppData)
//               (onSuccess   : ClientData => Callback,
//                onFailure   : ErrorMsg => Callback): Callback =
//    cp(remoteInit).mergeFailure.apply(
//      (),
//      i => {
//        val s = ProjectState.init(initAppData.project, initAppData.projectMetaData)
//        def cd = new ClientData(s)
//        onSuccess(cd)
//      },
//      onFailure)
}