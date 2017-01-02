package shipreq.webapp.client.project.app.root

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.univeq._
import scala.scalajs.js
import scalacss.ScalaCssReact._
import shipreq.base.util.Intersection
import shipreq.webapp.client.base.ui.semantic.{Colour, Dropdown, Header, Icon, JQuery, UsesSemanticUiManually}
import shipreq.webapp.client.project.app.Style.{home => *}
import Routes.{Page, RouterCtl}

object ProjectIndex {

  sealed abstract class Item(final val title   : String,
                             final val icon    : Icon,
                             final val subtitle: String)
  object Item {

    sealed abstract class WithPage(title   : String,
                                   icon    : Icon,
                                   val page: Page,
                                   subtitle: String) extends Item(title, icon, subtitle)

    case object ReqTable    extends WithPage("Req Table"        , Icon.Cubes         , Page.ReqTable   , "View and edit all reqs.")
    case object ReqDetail   extends Item    ("Req Lookup"       , Icon.Cube          ,                   "View and edit a single req.")
    case object ImpGraph    extends WithPage("Implication Graph", Icon.ShareAlternate, Page.ImpGraph   , "View project-wide implication.")
    case object CfgFields   extends WithPage("Fields"           , Icon.ListLayout    , Page.CfgFields  , "Configure fields that each type of req has.")
    case object CfgIssues   extends WithPage("Issues"           , Icon.WarningSign   , Page.CfgIssues  , "Configure types and causes of issues.")
    case object CfgReqTypes extends WithPage("Req Types"        , Icon.Inbox         , Page.CfgReqTypes, "Configure types of reqs.")
    case object CfgTags     extends WithPage("Tags"             , Icon.Tags          , Page.CfgTags    , "Configure attributes for content and organisation.")

    implicit def univEq: UnivEq[Item] = UnivEq.derive

    val ToPage: Intersection[Item, Page] =
      Intersection[Item, Page] {
        case w: WithPage => Some(w.page)
        case ReqDetail   => None
      } {
        case Page.ReqTable     => Some(ReqTable)
        case Page.ImpGraph     => Some(ImpGraph)
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
      NonEmptyVector(ReqTable, ReqDetail, ImpGraph))

    case object Configuration extends Category(
      "Configuration", Icon.Setting, Colour.Yellow, Colour.Grey,
      NonEmptyVector(CfgFields, CfgIssues, CfgReqTypes, CfgTags))

    implicit def univEq: UnivEq[Category] = UnivEq.derive

    //val All = AdtMacros.adtValuesManual[Category](
    val All = NonEmptyVector[Category](
      Content, Configuration)
  }

  def dropdownItems(active: Option[Item], rc: RouterCtl): Dropdown.Items =
    Category.All.iterator.flatMap(c =>
      Iterator.single(Dropdown.Item.Header(c.title)) ++
      c.items.iterator.flatMap(i =>
        if (active.exists(_ ==* i))
          Dropdown.Item.Div(i.title, Dropdown.ItemState.Active) :: Nil
        else Item.ToPage.getOption(i) match {
          case Some(p) => Dropdown.Item.Link(rc.link(p)(i.title)) :: Nil
          case None    => Nil
        }
      )
    ).toList

  case class Props(reqLookup: ReqLookupPrompt.Props, rc: RouterCtl) {
    @inline def render = Component(this)
  }

  @UsesSemanticUiManually
  final class Backend($: BackendScope[Props, Unit]) {

    val headerStyle = Header.Style(Header.Type.H3, Header.Attr.Dividing, other = *.cardHeader)

    private val dimIt = "xd"

    def enableDimmer: Callback =
      Callback {
        val opt = js.Dynamic.literal(on = "hover")
        JQuery($.getDOMNode()).find("." + dimIt).dimmer(opt)
      }

    def renderCard(p: Props, cat: Category, item: Item): TagMod = {
      val base = <.div(^.cls := "ui card " + (cat.cardColour.cls: String))

      val iconCont = <.div(^.cls := "content", *.cardIconCont)

      val icon = item.icon.withColour(cat.iconColour).tag(*.cardIcon)

      val contentTag =
        <.div(^.cls := "content",
          <.div(^.cls := "header", item.title),
          <.div(^.cls := "description", item.subtitle))

      item match {
        case i: Item.WithPage =>
          base(
            *.linkCard,
            p.rc.setOnLinkClick(i.page),
            iconCont(icon),
            contentTag)

        case i@Item.ReqDetail =>
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

      val cards = <.div(^.cls := "ui cards three", *.cardsCont,
        cat.items.whole.map(renderCard(p, cat, _)))

      header + cards
    }

    def render(p: Props): ReactElement =
      <.section(
        Category.All.foldLeft(EmptyTag)((q, c) => q + renderCategory(p, c)))
  }

  val Component = ReactComponentB[Props]("Index")
    .renderBackend[Backend]
    .componentDidMount(_.backend.enableDimmer)
    .build
}
