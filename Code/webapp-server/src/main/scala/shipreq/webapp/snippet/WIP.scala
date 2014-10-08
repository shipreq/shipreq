package shipreq.webapp.snippet

import net.liftweb.util.Helpers._
import shipreq.webapp.shared.protocol._
import shipreq.webapp.lib.ServerProtocol

import shipreq.webapp.shared.data._
import shipreq.webapp.shared.data.delta._

class WIP {

  var rev = 99

  val create = ServerProtocol.routine(Routines.CustReqTypeOps.Create)(vs => {
    val (mnemonic, name, impReq) = vs
    println(s"TODO: create $vs")
    None
  })

  val update = ServerProtocol.routine(Routines.CustReqTypeOps.Update)(vs => {
    val (id, (mnemonic, name, impReq)) = vs
    println(s"TODO: update $vs")
    None
  })

  val softDelete = ServerProtocol.routine(Routines.CustReqTypeOps.SoftDelete)(id => {
    println(s"TODO: softDelete $id")
    rev += 1
    Thread.sleep(1500)
    val dg = RemoteDeltaG(Partition.CustReqType, Rev(0), Rev(rev))(id :: Nil, Nil)
    val d: RemoteDelta = dg :: Nil
    d
  })

  val hardDelete = ServerProtocol.routine(Routines.CustReqTypeOps.HardDelete)(id => {
    println(s"TODO: hardDelete $id")
    None
  })

  val restore = ServerProtocol.routine(Routines.CustReqTypeOps.Restore)(id => {
    println(s"TODO: restore $id")
    None
  })

  def render = {
    val pg = Routines.ForCfgReqType(create, update, softDelete, hardDelete, restore)
    val js = ServerProtocol.invokeClientHtml(JsEntryPoint.reactExamples)(pg)
    "*" #> js
  }
}
