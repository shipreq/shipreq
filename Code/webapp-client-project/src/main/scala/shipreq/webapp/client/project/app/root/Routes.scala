package shipreq.webapp.client.project.app.root

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.{RouterCtl => RouterCtl_, _}
import japgolly.scalajs.react.vdom.Implicits._
import monocle._
import monocle.macros._
import scala.annotation.elidable
import shipreq.base.util.univeq._
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.data.{ExternalPubid, ReqType, ReqTypePos}
import shipreq.webapp.base.text.PlainText

object Routes {

  type RouterCtl = RouterCtl_[Page]

  sealed trait Page
  object Page {
    case object CfgFields   extends Page
    case object CfgIssues   extends Page
    case object CfgReqTypes extends Page
    case object CfgTags     extends Page
    case object ImpGraph    extends Page
    case object Index       extends Page
    case object ReqTable    extends Page

    case class ReqDetail(pubid: ExternalPubid) extends Page {
      @elidable(elidable.INFO)
      override def toString = s"ReqDetail(${PlainText pubid pubid})"
    }
    object ReqDetail {
      val stringPrism: Prism[String, ReqDetail] =
        ExternalPubid.StringPrism ^<-> GenIso.fields[ReqDetail].reverse
    }

    implicit def equality: UnivEq[Page] = UnivEq.derive
    implicit def reusability: Reusability[Page] = Reusability.byUnivEq

    val title: Page => List[String] = {
      case Index        => Nil
      case ReqTable     => "ReqTable" :: Nil
      case ReqDetail(p) => PlainText.pubid(p) :: Nil
      case ImpGraph     => ProjectIndex.Item.ImpGraph.title :: Nil
      case CfgFields    => "Config " + ProjectIndex.Item.CfgFields  .title :: Nil
      case CfgIssues    => "Config " + ProjectIndex.Item.CfgIssues  .title :: Nil
      case CfgReqTypes  => "Config " + ProjectIndex.Item.CfgReqTypes.title :: Nil
      case CfgTags      => "Config " + ProjectIndex.Item.CfgTags    .title :: Nil
    }

    def sampleValues = NonEmptyVector[Page](
      CfgFields,
      CfgIssues,
      CfgReqTypes,
      CfgTags,
      ImpGraph,
      Index,
      ReqDetail(ExternalPubid(ReqType.Mnemonic("A"), ReqTypePos(1))),
      ReqTable)
  }

  def routerConfig(rootInstance: LoadedRoot) =
    RouterConfigDsl[Page].buildConfig { dsl =>
      import dsl._

      def render(page: Page, r: RouterCtl) =
        rootInstance.Component(LoadedRoot.Props(page, r))

      val dynPage = dynRenderR((page: Page, r) => render(page, r))

      def staticPage(route: StaticDsl.Route[Unit], page: Page) =
        staticRoute(route, page) ~> renderR(r => render(page, r))

      val reqTablePath = "#reqs"

      def reqDetailRoute =
        dynamicRouteCT(reqTablePath / remainingPath.pmapL(Page.ReqDetail.stringPrism)) ~> dynPage autoCorrect

      def title(p: Page): String =
        WebappConfig.makePageTitle(Page.title(p) :+ rootInstance.cd.project().name: _*)

      ( staticPage(dsl.root       , Page.Index      )
      | staticPage(reqTablePath   , Page.ReqTable   )
      | staticPage("#impgraph"    , Page.ImpGraph   )
      | staticPage("#cfg/fields"  , Page.CfgFields  )
      | staticPage("#cfg/issues"  , Page.CfgIssues  )
      | staticPage("#cfg/reqtypes", Page.CfgReqTypes)
      | staticPage("#cfg/tags"    , Page.CfgTags    )
      | reqDetailRoute
      | trimSlashes
      ).notFound(redirectToPage(Page.Index)(Redirect.Replace))
        .setTitle(title)
        .verify(Page.sampleValues.head, Page.sampleValues.tail: _*)
    }
}
