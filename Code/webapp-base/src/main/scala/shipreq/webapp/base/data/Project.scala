package shipreq.webapp.base.data

import monocle.Lens
import monocle.macros.Lenser
import shipreq.base.util.Must

case class RevAnd[D](rev: Rev, data: D)

object RevAnd {
  def _data[D] = Lens((_: RevAnd[D]).data)(b => _.copy(data = b))
}

object Project {
  private[this] def l = Lenser[Project]
  val _customIssueTypes = l(_.customIssueTypes)
  val _customReqTypes   = l(_.customReqTypes)
  val _fields           = l(_.fields)
  val _tags             = l(_.tags)
}

final case class Project(customIssueTypes: RevAnd[CustomIssueTypeIMap],
                         customReqTypes:   RevAnd[CustomReqTypeIMap],
                         fields:           RevAnd[FieldSet],
                         tags:             RevAnd[TagTree]) {

  import japgolly.nyaya._
  this assertSatisfies DataProp.project

  def rev =
    customIssueTypes.rev +
    customReqTypes  .rev +
    fields          .rev +
    tags            .rev

  override def toString =
    Stream(customIssueTypes, customReqTypes, fields, tags)
      .map("\n    " + _.toString.replace(" -> ", " → "))
      .mkString("Project(", "", "\n)")

  def reqType(i: ReqType.Id): Must[ReqType] =
    i.foldId[Must[ReqType]](s => s, customReqTypes.data.apply)

  lazy val reqTypes: Stream[ReqType] =
    (customReqTypes.data.values.toStream: Stream[ReqType]) #:::
    (StaticReqType.valueStream          : Stream[ReqType])


  // ------------------------------------------------------------------------------
  import SCRATCH._ // TODO Hardcoded ↓
  import shipreq.base.util.IMap

  val publicReqIdRegister: PublicReqId.Register = ???
  val reqs               : IMap[Req.Id, Req]    = ???
  val reqCodeTrie        : ReqCode.Trie         = ???
  val reqFieldData       : ReqFieldData         = ???

  val reqCodesPerTarget = ReqCode.Trie.inverse(reqCodeTrie)

  def publicReqId_reqId(id: PublicReqId): Option[Req.Id] = {
    val v = publicReqIdRegister(id.reqTypeId)
    val i = id.pos.value - 1
    try {
      Some(v(i))
    } catch {
      case _: IndexOutOfBoundsException => None
    }
  }

  def publicReqId_req(id: PublicReqId): Option[Req] =
    publicReqId_reqId(id) flatMap req

  def req(id: Req.Id): Option[Req] =
    reqs.get(id)

}