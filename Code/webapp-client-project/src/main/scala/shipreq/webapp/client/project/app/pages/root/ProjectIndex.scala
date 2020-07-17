package shipreq.webapp.client.project.app.pages.root

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.scalajs.js
import scalacss.ScalaCssReact._
import shipreq.base.util.Intersection
import shipreq.webapp.base.issue.IssueCount
import shipreq.webapp.base.ui.semantic.{Colour, Dropdown, Header, Icon, JQuery, UsesSemanticUiManually}
import shipreq.webapp.client.project.app.Style.{home => *}
import shipreq.webapp.client.project.app.pages.root.Routes.{Page, RouterCtl}

object ProjectIndex {

  sealed abstract class Item(final val title   : String,
                             final val icon    : Icon,
                             final val subtitle: String) {

    final val iconAndTitle: TagMod =
      <.span(icon.tag, " " + title)
  }

  object Item {

    sealed abstract class WithPage(title   : String,
                                   icon    : Icon,
                                   val page: Page,
                                   subtitle: String) extends Item(title, icon, subtitle)

    case object ReqTable    extends WithPage("Req Table" , Icon.Cubes         , Page.ReqTable   , "View and edit reqs.")
    case object ReqDetail   extends Item    ("Req Lookup", Icon.Cube          ,                   "View and edit a single req.")
    case object Issues      extends WithPage("Issues"    , Icon.WarningSign   , Page.Issues     , "View and resolve outstanding issues.")
    case object ReqGraph    extends WithPage("Req Graph" , Icon.ShareAlternate, Page.ReqGraph   , "View a graph of reqs.")
    case object CfgFields   extends WithPage("Fields"    , Icon.ListLayout    , Page.CfgFields  , "Configure fields that each type of req has.")
    case object CfgIssues   extends WithPage("Issues"    , Icon.WarningSign   , Page.CfgIssues  , "Configure types and causes of issues.")
    case object CfgReqTypes extends WithPage("Req Types" , Icon.Inbox         , Page.CfgReqTypes, "Configure types of reqs.")
    case object CfgTags     extends WithPage("Tags"      , Icon.Tags          , Page.CfgTags    , "Configure attributes for content and organisation.")

    implicit def univEq: UnivEq[Item] = UnivEq.derive

    val ToPage: Intersection[Item, Page] =
      Intersection[Item, Page] {
        case w: WithPage => Some(w.page)
        case ReqDetail   => None
      } {
        case Page.ReqTable     => Some(ReqTable)
        case Page.ReqGraph     => Some(ReqGraph)
        case Page.Issues       => Some(Issues)
        case Page.CfgFields    => Some(CfgFields)
        case Page.CfgIssues    => Some(CfgIssues)
        case Page.CfgReqTypes  => Some(CfgReqTypes)
        case Page.CfgTags      => Some(CfgTags)
        case Page.ReqDetail(_) => Some(ReqDetail)
        case Page.Index        => None
      }
  }

  sealed abstract class Category(final val title     : String,
                                 final val icon      : Icon,
                                 final val cardColour: Colour,
                                 final val iconColour: Colour,
                                 final val items     : NonEmptyVector[Item])
  object Category {
    import Item._

    case object Content extends Category(
      "Content", Icon.FileTextOutline, Colour.Blue, Colour.Default,
      NonEmptyVector(
        ReqTable,
        ReqDetail,
        ReqGraph,
        Issues,
      ))

    case object Configuration extends Category(
      "Configuration", Icon.Setting, Colour.Yellow, Colour.Grey,
      NonEmptyVector(
        CfgFields,
        CfgIssues,
        CfgReqTypes,
        CfgTags,
      ))

    implicit def univEq: UnivEq[Category] = UnivEq.derive

    val All = AdtMacros.adtValuesManually[Category](
      Content, Configuration)
  }

  def dropdownItems(active: Option[Item], rc: RouterCtl): Dropdown.Items =
    Category.All.iterator.flatMap(c =>
      Iterator.single(Dropdown.Item.Header(c.title)) ++
      c.items.iterator.flatMap(i =>
        if (active.exists(_ ==* i))
          Dropdown.Item.Div(i.iconAndTitle, Dropdown.ItemState.Active) :: Nil
        else Item.ToPage.getOption(i) match {
          case Some(p) => Dropdown.Item.Link(rc.link(p)(i.iconAndTitle)) :: Nil
          case None    => Nil
        }
      )
    ).toList

  final case class Props(issueCount: IssueCount,
                         reqLookup : ReqLookupPrompt.Props,
                         rc        : RouterCtl) {
    @inline def render = Component(this)
  }

  @UsesSemanticUiManually
  final class Backend($: BackendScope[Props, Unit]) {

    val headerStyle = Header.Style(Header.Type.H3, Header.Attr.Dividing, other = *.cardHeader)

    private val dimIt = "xd"

    def enableDimmer: Callback =
      $.getDOMNode.map(_.toElement.foreach { node =>
        val opt = js.Dynamic.literal(on = "hover")
        JQuery(node).find("." + dimIt).dimmer(opt)
      })

    def renderCard(p: Props, cat: Category, item: Item): TagMod = {
      val base = <.div(^.cls := "ui card " + (cat.cardColour.cls: String))

      val iconCont = <.div(^.cls := "content", *.cardIconCont)

      val icon = item.icon.withColour(cat.iconColour).tag(*.cardIcon)

      def renderWithPage(i: Item.WithPage, content: VdomTag) =
        base(
          *.linkCard,
          p.rc.setOnLinkClick(i.page),
          iconCont(icon),
          content)

      val contentTag =
        <.div(^.cls := "content",
          <.div(^.cls := "header", item.title),
          <.div(^.cls := "description", item.subtitle))

      item match {
        case i: Item.Issues.type if p.issueCount.value > 0 =>
          val countTag = <.div(^.cls := "floating ui red circular label", p.issueCount.value)
          renderWithPage(i, contentTag(countTag))

        case i: Item.WithPage =>
          renderWithPage(i, contentTag)

        case Item.ReqDetail =>
          base(
            iconCont(^.cls := ("blurring " + dimIt),
              <.div(^.cls := "ui inverted dimmer",
                <.div(^.cls := "content",
                  <.div(^.cls := "center",
                    <.div(^.cls := "ui search",
                      <.div(^.cls := "ui icon input",
                        p.reqLookup.render,
                        Icon.Search.tag))))),
              icon),
            contentTag)
      }
    }

    def renderCategory(p: Props, cat: Category): TagMod = {
      val header = Header(headerStyle, cat.icon, cat.title)

      val cards = <.div(^.cls := "ui cards four", *.cardsCont,
        cat.items.whole.toTagMod(renderCard(p, cat, _)))

      TagMod(header, cards)
    }

    def render(p: Props): VdomElement =
      <.section(
        Category.All.whole.toTagMod(renderCategory(p, _)))
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .componentDidMount(_.backend.enableDimmer)
    .build
}
