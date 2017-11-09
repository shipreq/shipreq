package shipreq.webapp.client.project.app.state

import japgolly.scalajs.react.extra.{Broadcaster, Px, Reusability}
import japgolly.scalajs.react.{Callback, CallbackTo}
import scalaz.\/
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.event.VerifiedEvent
import shipreq.webapp.base.protocol.{ProjectSpaProtocols, ServerSideProc}
import shipreq.webapp.base.protocol.{ClientProtocol, ServerSideProcInvoker}

class ClientData(initialState: ProjectState) extends Broadcaster[Changes] {

  protected val mutableState = new ProjectState.Mutable(initialState)

  // Broadcast changes
  mutableState.addListener((ves, ps1, ps2) =>
    VerifiedEvent.NonEmptySeq.maybe(ves) match {
      case Some(ne) => broadcast(Changes(ne, ps1.project, ps2.project))
      case None     => Callback.empty // Events queued but not applied
    })

  // Old API
  final def project()                               : Project             = mutableState.pxProject.value()
  final def projectMetaData()                       : ProjectMetaData     = mutableState.state().projectMetaData
  final val pxProject                               : Px[Project]         = mutableState.pxProject
  final val projectCB                               : CallbackTo[Project] = pxProject.toCallback
  final def applyEventSeqCB(ves: VerifiedEvent.Seq) : Callback            = mutableState.applyEventSeqCB(ves)

  final def serverSideProcToEvents[I](cp: ClientProtocol,
                                      proc: ServerSideProc[I, ErrorMsg \/ VerifiedEvent.Seq]): ServerSideProcInvoker[I, ErrorMsg, VerifiedEvent.Seq] =
    cp(proc)
      .mergeFailure
      .onSuccess((ves, s) => s << mutableState.applyEventSeqCB(ves))
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object ClientData {

  @inline implicit def reusability = Reusability.byRef[ClientData]

  def initAsync(cp          : ClientProtocol,
                remoteInit  : ProjectSpaProtocols.InitAsync.Instance)
               (onSuccess   : ClientData => Callback,
                onFailure   : ErrorMsg => Callback): Callback =
    cp(remoteInit).mergeFailure.apply(
      (),
      i => {
        val s = ProjectState.init(i.project, i.projectMetaData, i.latestEventOrd)
        def cd = new ClientData(s)
        onSuccess(cd)
      },
      onFailure)
}