package shipreq.webapp.client

import org.scalajs.dom
import shipreq.webapp.client.delta._
import shipreq.webapp.shared.data.Project
import shipreq.webapp.shared.data.delta.{Rev, RemoteDeltas}
import japgolly.scalajs.react.experiment.Broadcaster

final class ClientData(initial: Project) extends Broadcaster[LocalDeltas] {

  private var p = initial

  @inline def project = p

  def update(d: RemoteDeltas): Unit = {
    RemoteDelta(p, d) match {
      case Applied(p2, d2) =>
        p = p2
        broadcast(d2)
      case CouldntApply =>
        dom.console.error(s"Update failed.\n\nΠ: $p\n\nΔ: $d") // TODO fix or at least remove in prod
    }
  }
}

object ClientData {

  // TODO Restrict access to global data so that components don't have direct access. private[...]
  val GLOBAL = { // TODO Global client state given fake bullshit data
    import shipreq.webapp.shared.data._
    import CustReqType.Id
    implicit def autoMnemonic(s: String) = ReqType.Mnemonic(s)
    val list = List(
      CustReqType(Id(1), "CO", Set.empty, "Constraint", ImplicationNotRequired, Alive),
      CustReqType(Id(2), "MF", Set.empty, "Major Feature", ImplicationNotRequired, Alive),
      CustReqType(Id(3), "FR", Set.empty, "Functional Requirement", ImplicationNotRequired, Alive),
      CustReqType(Id(4), "BR", Set.empty, "Business Rule", ImplicationNotRequired, Alive),
      CustReqType(Id(5), "DD", Set("DA", "DDF"), "Data Definition", ImplicationNotRequired, Dead),
      CustReqType(Id(6), "SI", Set.empty, "Solution Idea", ImplicationRequired, Dead)
    )
    //val map = list.map(i => i.id -> i).toMap
    val rev = Rev(6)
    new ClientData(Project(CustomReqTypes(rev, list)))
  }
}