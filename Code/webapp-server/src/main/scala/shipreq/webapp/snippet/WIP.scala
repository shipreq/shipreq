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
    import shipreq.webapp.shared.UnsafeTypes._

    val customImplTypes = DataSet[CustomIncmpTypeAndId](10, List(
      CustomIncmpType(1, "TODO", "Something you need To Do.", Alive),
      CustomIncmpType(2, "TBD", "To Be Decided.", Alive)))

    val customReqTypes = DataSet[CustomReqTypeAndId](20, List(
        CustomReqType(1, "CO", Set.empty, "Constraint", ImplicationNotRequired, Alive),
        CustomReqType(2, "MF", Set.empty, "Major Feature", ImplicationNotRequired, Alive),
        CustomReqType(3, "FR", Set.empty, "Functional Requirement", ImplicationRequired, Alive),
        CustomReqType(4, "BR", Set.empty, "Business Rule", ImplicationNotRequired, Alive),
        CustomReqType(5, "DD", Set("DA", "DDF"), "Data Definition", ImplicationNotRequired, Dead),
        CustomReqType(6, "SI", Set.empty, "Solution Idea", ImplicationRequired, Dead)))

    new Project(customImplTypes, customReqTypes)
  }

  var p = newProject

  val projectInit = ServerProtocol.routine(Routines.ProjectInit)(_ => p)

  // -------------------------------------------------------------------------------------------------------------------
  val reqCrud = {

    def upd(id: CustomReqType.Id, f: CustomReqType => CustomReqType) =
      mod(_.map(c => if (c.id == id) f(c) else c))

    def mod1(f: List[CustomReqType] => List[CustomReqType]): Option[Rev] = {
      val c = p.customReqTypes
      val a = c.data
      val b = f(a)
      if (a == b)
        None
      else {
        val rev = c.rev.succ
        p = p.copy(customReqTypes = DataSet[CustomReqTypeAndId](rev, b))
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

    ServerProtocol.routine(Routines.CustomReqTypeCrud)({
      case CrudAction.Create(v)    =>
        val (mnemonic, name, imp) = v
        val id = CustomReqType.Id(p.customReqTypes.data.map(_.id.value).max + 1)
        val n = CustomReqType(id, mnemonic, Set.empty, name, imp, Alive)
        mod(n :: _)

      case CrudAction.Update(id, v) =>
        val (mnemonic, name, imp) = v
        upd(id, o => CustomReqType(id, mnemonic, (o.oldMnemonics + o.mnemonic) - mnemonic, name, imp, Alive))

      case CrudAction.Delete(id, HardDel) => mod(_.filterNot(_.id == id))
      case CrudAction.Delete(id, SoftDel) => upd(id, _.copy(alive = Dead))
      case CrudAction.Delete(id, Restore) => upd(id, _.copy(alive = Alive))
    })
  }

  // -------------------------------------------------------------------------------------------------------------------
  // TODO Another copy/paste/search/replace
  val incmpCrud = {

    def upd(id: CustomIncmpType.Id, f: CustomIncmpType => CustomIncmpType) =
      mod(_.map(c => if (c.id == id) f(c) else c))

    def mod1(f: List[CustomIncmpType] => List[CustomIncmpType]): Option[Rev] = {
      val c = p.customIncmpTypes
      val a = c.data
      val b = f(a)
      if (a == b)
        None
      else {
        val rev = c.rev.succ
        p = p.copy(customIncmpTypes = DataSet[CustomIncmpTypeAndId](rev, b))
        Some(rev)
      }
    }

    def mod(f: List[CustomIncmpType] => List[CustomIncmpType]): RemoteDelta = {
      val p1 = p
      Thread.sleep(new java.util.Random().nextInt(120)+100)
      mod1(f).map(rev => {
        val m1 = p1.customIncmpTypes.data.map(c => c.id -> c).toMap
        val m2 = p.customIncmpTypes.data.map(c => c.id -> c).toMap
        val delIds = (m1.keySet -- m2.keySet).toList
        val updates = m2.toStream.filter{ case (k,v) => !m1.contains(k) || m1(k) != v }.map(_._2).toList
        RemoteDeltaG(Partition.CustomIncmpTypes, rev, rev)(delIds, updates)
      }).toList
    }

    ServerProtocol.routine(Routines.CustomIncmpTypeCrud)({
      case CrudAction.Create(v)    =>
        val (key, desc) = v
        val id = CustomIncmpType.Id(p.customIncmpTypes.data.map(_.id.value).max + 1)
        val n = CustomIncmpType(id, key, desc, Alive)
        mod(n :: _)

      case CrudAction.Update(id, v) =>
        val (key, desc) = v
        upd(id, o => CustomIncmpType(id, key, desc, Alive))

      case CrudAction.Delete(id, HardDel) => mod(_.filterNot(_.id == id))
      case CrudAction.Delete(id, SoftDel) => upd(id, _.copy(alive = Dead))
      case CrudAction.Delete(id, Restore) => upd(id, _.copy(alive = Alive))
    })
  }

  // -------------------------------------------------------------------------------------------------------------------
  def render = {
    val pg = Routines.ForCfgReqType(projectInit, incmpCrud, reqCrud)
    val js = ServerProtocol.invokeClientHtml(JsEntryPoint.reactExamples)(pg)
    "*" #> js
  }
}
