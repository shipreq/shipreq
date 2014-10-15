package shipreq.webapp.snippet

import net.liftweb.util.Helpers._
import shipreq.webapp.shared.protocol._
import shipreq.webapp.lib.ServerProtocol
import shipreq.webapp.shared.data._
import shipreq.webapp.shared.data.delta._
import DeletionAction._
import shipreq.webapp.util.QuietException

class WIP {

  def newProject = {
    import shipreq.webapp.shared.data._
    import CustomReqType.Id
    implicit def autoMnemonic(s: String) = ReqType.Mnemonic(s)
    val list = List(
      CustomReqType(Id(1), "CO", Set.empty, "Constraint", ImplicationNotRequired, Alive),
      CustomReqType(Id(2), "MF", Set.empty, "Major Feature", ImplicationNotRequired, Alive),
      CustomReqType(Id(3), "FR", Set.empty, "Functional Requirement", ImplicationRequired, Alive),
      CustomReqType(Id(4), "BR", Set.empty, "Business Rule", ImplicationNotRequired, Alive),
      CustomReqType(Id(5), "DD", Set("DA", "DDF"), "Data Definition", ImplicationNotRequired, Dead),
      CustomReqType(Id(6), "SI", Set.empty, "Solution Idea", ImplicationRequired, Dead)
    )
    //val map = list.map(i => i.id -> i).toMap
    val rev = Rev(100)
    new Project(CustomReqTypes(rev, list))
  }

  var p = newProject

  val projectInit = ServerProtocol.routine(Routines.ProjectInit)(_ => p)

  val crud = ServerProtocol.routine(Routines.CustomReqTypeCrud)({
    case CrudAction.Create(v)    =>
      val (mnemonic, name, imp) = v
      val id = CustomReqType.Id(p.customReqTypes.data.map(_.id.value).max + 1)
      val n = CustomReqType(id, mnemonic, Set.empty, name, imp, Alive)
      mod(n :: _)

    case CrudAction.Update(id, v) =>
      val (mnemonic, name, imp) = v
      upd(id, o => CustomReqType(id, mnemonic, (o.oldMnemonics + o.mnemonic) - mnemonic, name, imp, Alive))

    case CrudAction.Delete(id, HardDel) =>
      mod(_.filterNot(_.id == id))

    case CrudAction.Delete(id, SoftDel) =>
      upd(id, _.copy(alive = Dead))

    case CrudAction.Delete(id, Restore) =>
      upd(id, _.copy(alive = Alive))
  })

  def upd(id: CustomReqType.Id, f: CustomReqType => CustomReqType) =
    mod(_.map(c => if (c.id == id) f(c) else c))

  def mod1(f: List[CustomReqType] => List[CustomReqType]): Option[Rev] = {
    val a = p.customReqTypes.data
    val b = f(a)
    if (a == b)
      None
    else {
      val rev = p.customReqTypes.rev.succ
      p = Project(CustomReqTypes(rev, b))
      Some(rev)
    }
  }

  def mod(f: List[CustomReqType] => List[CustomReqType]): RemoteDelta = {
    val p1 = p
    Thread.sleep(new java.util.Random().nextInt(120)+100)
    mod1(f).map(rev => {
      val m1 = p1.customReqTypes.data.map(c => c.id -> c).toMap
      val m2 = p.customReqTypes.data.map(c => c.id -> c).toMap
      val delIds = (m1.keySet -- m2.keySet).toList
      val updates = m2.toStream.filter{ case (k,v) => !m1.contains(k) || m1(k) != v }.map(_._2).toList
      RemoteDeltaG(Partition.CustomReqTypes, rev, rev)(delIds, updates)
    }).toList
  }

  def render = {
    val pg = Routines.ForCfgReqType(projectInit, crud)
    val js = ServerProtocol.invokeClientHtml(JsEntryPoint.reactExamples)(pg)
    "*" #> js
  }
}
