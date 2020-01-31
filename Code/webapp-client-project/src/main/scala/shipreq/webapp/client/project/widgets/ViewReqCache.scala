package shipreq.webapp.client.project.widgets

import japgolly.microlibs.utils.Memo
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^.VdomTag
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.ProjectText
import shipreq.webapp.client.project.lib.DataReusability._

final case class ViewReqDataCache(private[ViewReqDataCache] val project: Project) {

  private[this] val cache: FilterDead => ReqId => ViewReq.Data =
    FilterDead.memo { fd =>
      Memo { reqId =>
        ViewReq.Data.fromProject(reqId, project, fd)
      }
    }

  def apply(fd: FilterDead): ReqId => ViewReq.Data =
    cache(fd)
}

object ViewReqDataCache {
  implicit val reusability: Reusability[ViewReqDataCache] =
    Reusability.byRef || Reusability.derive
}

// =====================================================================================================================

final case class ViewReqCache[Ctx <: ProjectText.Context, A](dataCache: ViewReqDataCache,
                                                             private[ViewReqCache] val pt: ProjectText[ProjectText.Context, A]) {

  private[this] val cache: FilterDead => ReqId => ViewReq[A] =
    FilterDead.memo { fd =>
      val f = dataCache(fd)
      Memo { reqId =>
        f(reqId)(pt)
      }
    }

  def apply(fd: FilterDead): ReqId => ViewReq[A] =
    cache(fd)
}

object ViewReqCache {

  type ToVdom[Ctx <: ProjectText.Context] = ViewReqCache[Ctx, VdomTag]

  implicit def reusability[Ctx <: ProjectText.Context, A]: Reusability[ViewReqCache[Ctx, A]] =
    Reusability.byRef || Reusability.derive
}
