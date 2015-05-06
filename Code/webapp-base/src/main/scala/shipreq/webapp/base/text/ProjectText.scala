package shipreq.webapp.base.text

import shipreq.base.util.{Must, UnivEq}
import shipreq.webapp.base.data._

object ProjectText {
  @inline def apply[Out](project: Project, _format: Text.AnyOptional => Out): ProjectText[Out] =
    new ProjectText[Out](project) {
      override val format = _format
    }
}

abstract class ProjectText[Out](project: Project) {
  import UnivEq.{mutableHashMapMemo => memo}

  val format: Text.AnyOptional => Out

  val format1: Text.AnyNonEmpty => Out =
    nev => format(nev.whole)

  private val _reqTitle: Req => Out = {
    case r: GenericReq => format(r.title)
  }

  val reqTitle: Req => Out = {
    val memo = new scala.collection.mutable.HashMap[ReqId, Out]
    req => memo.getOrElseUpdate(req.id, _reqTitle(req))
  }

  private val reqCodeGroupTitleMemo =
    new scala.collection.mutable.HashMap[ReqCodeId, Out]

  def reqCodeGroupTitle(id: ReqCodeId, g: ReqCodeGroup): Out =
    reqCodeGroupTitleMemo.getOrElseUpdate(id, format(g.title))

  def reqTitleById(id: ReqId): Must[Out] =
    project.reqs.data.reqM(id) map reqTitle

  private val _customTextField: CustomField.Text.Id => ReqId => Option[Out] =
    fid => {
      val m = project.reqFieldData.data.text.getOrElse(fid, Map.empty)
      m.get(_) map format1
    }

  val customTextField: CustomField.Text.Id => ReqId => Option[Out] =
    memo { fid => val g = _customTextField(fid); memo(g) }
}
