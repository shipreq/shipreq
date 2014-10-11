package shipreq.webapp.snippet

import net.liftweb.util.Helpers._
import shipreq.webapp.shared.protocol._
import shipreq.webapp.lib.ServerProtocol
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.data.delta._
import DeletionAction._
import shipreq.webapp.util.QuietException

class WIP {

  var rev = 99

  val crud = ServerProtocol.routine(Routines.CustomReqTypeCrud)({
    case CrudAction.Create(v)    =>
      println(s"TODO: create $v"); Nil

    case CrudAction.Update(id, v) =>
      println(s"\nTODO: update $id $v\n")
      throw QuietException

    case CrudAction.Delete(id, HardDel) =>
      println(s"TODO: Hard delete $id"); Nil
      rev += 1
      Thread.sleep(1500)
      val dg = RemoteDeltaG(Partition.CustomReqTypes, Rev(0), Rev(rev))(id :: Nil, Nil)
      val d: RemoteDelta = dg :: Nil
      d

    case CrudAction.Delete(id, SoftDel) =>
      println(s"\nTODO: Soft delete $id\n")
      throw QuietException

    case CrudAction.Delete(id, Restore) =>
      println(s"TODO: Restore $id"); Nil
  })

  def render = {
    val pg = Routines.ForCfgReqType(crud)
    val js = ServerProtocol.invokeClientHtml(JsEntryPoint.reactExamples)(pg)
    "*" #> js
  }
}
