package shipreq.webapp.base.data

import monocle.Lens
import monocle.macros.Lenser

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

  def reqType(i: ReqType.Id): Option[ReqType] =
    i.foldId(Some(_), customReqTypes.data.get)
}