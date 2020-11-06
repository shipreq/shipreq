package shipreq.webapp.client.project.app.pages.root

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.router.{RouterCtl => RouterCtl_, _}
import japgolly.scalajs.react.{Callback, Reusability}
import monocle._
import monocle.macros._
import shipreq.base.util.Url
import shipreq.webapp.base.config.WebappConfig
import shipreq.webapp.base.util.GoogleAnalytics
import shipreq.webapp.member.project.data.{ExternalPubid, ReqType, ReqTypePos}
import shipreq.webapp.member.project.text.PlainText
import shipreq.webapp.member.project.util.DataReusability._

object Routes {

  type RouterCtl = RouterCtl_[Page]

  sealed trait Page
  object Page {
    sealed abstract class HasStaticTitle(final val title: String) extends Page

    case object CfgFields   extends HasStaticTitle("Field Config")
    case object CfgIssues   extends HasStaticTitle("Issue Config")
    case object CfgReqTypes extends HasStaticTitle("Req Type Config")
    case object CfgTags     extends HasStaticTitle("Tag Config")
    case object ReqGraph    extends Page
    case object Issues      extends Page
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
      case Issues       => "Issues" :: Nil
      case ReqGraph     => ProjectIndex.Item.ReqGraph.title :: Nil
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
      ReqGraph,
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

      val reqTablePath = "#/reqs"

      def reqDetailRoute =
        dynamicRouteCT(reqTablePath / remainingPath.pmapL(Page.ReqDetail.stringPrism)) ~> dynPage autoCorrect

      def title(p: Page): String =
        WebappConfig.makePageTitle(Page.title(p) :+ rootInstance.unsafeProject().name: _*)

      val normalisation =
        rewritePathF { path =>
          var p = path.value

          // Ensure that a slash is between the leading hash and whatever-else
          if (p.length > 1 && p(0) == '#' && p(1) != '/')
            p = "#/" + p.drop(1)

          // Consolidate slashes
          if (p.contains("//"))
            p = p.replaceAll("/{2,}", "/")

          // Remove trailing slash
          if (p.endsWith("/"))
            p = p.dropRight(1)

          // Rewrite old cfg paths
          if (p.startsWith("#cfg/"))
            p = "#/config/" + p.drop(5)

          Option.when(p != path.value)(redirectToPath(p)(SetRouteVia.HistoryReplace))
        }

      ( staticPage(dsl.root           , Page.Index      )
      | staticPage(reqTablePath       , Page.ReqTable   )
      | staticPage("#/issues"         , Page.Issues     )
      | staticPage("#/reqgraph"       , Page.ReqGraph   )
      | staticPage("#/config/fields"  , Page.CfgFields  )
      | staticPage("#/config/issues"  , Page.CfgIssues  )
      | staticPage("#/config/reqtypes", Page.CfgReqTypes)
      | staticPage("#/config/tags"    , Page.CfgTags    )
      | normalisation
      | reqDetailRoute
      ).notFound(redirectToPage(Page.Index)(SetRouteVia.HistoryReplace))
        .setTitle(title)
        .onPostRender(trackPage)
        .verify(Page.sampleValues.head, Page.sampleValues.tail: _*)
    }

    private val trackPage: (Option[Page], Page) => Callback = {
      val root = Url.Relative("/project/<id>")
      val path: Page => Url.Relative = {
        case Page.Index        => root
        case Page.ReqTable     => root / "reqTable"
        case Page.ReqDetail(_) => root / "reqDetail"
        case Page.Issues       => root / "issues"
        case Page.ReqGraph     => root / "impGraph"
        case Page.CfgFields    => root / "cfg/fields"
        case Page.CfgIssues    => root / "cfg/issues"
        case Page.CfgReqTypes  => root / "cfg/reqTypes"
        case Page.CfgTags      => root / "cfg/tags"
      }
      GoogleAnalytics.onRouteChange(_, _)(path)
    }
}
