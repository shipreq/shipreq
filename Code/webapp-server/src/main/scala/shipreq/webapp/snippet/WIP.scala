package shipreq.webapp.snippet

import net.liftweb.util.Helpers._
import shipreq.webapp.shared.protocol._
import shipreq.webapp.lib.ServerProtocol

class WIP {

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
    None
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
