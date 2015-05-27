package hahaa

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router2._
import org.scalajs.dom._
import scala.scalajs.js
import scalaz.effect.IO
import scalaz.std.AllInstances._
import scalaz.syntax.bind.ToBindOps
import scalacss.Defaults._
import scalacss.ScalaCssReact._

import shipreq.base.util.{NonEmptyVector, NonEmptySet, UnivEq}
import shipreq.webapp.base.protocol.Routines
import shipreq.webapp.client.ClientData
import shipreq.webapp.client.app.ui
import shipreq.webapp.client.lib.ConsoleIO
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.DND
import shipreq.webapp.client.lib.HideDead

object ShipreqWip {

  sealed trait Page
  case object CfgFields   extends Page
  case object CfgIssues   extends Page
  case object CfgReqTypes extends Page
  case object CfgTags     extends Page
  case object ReqTable    extends Page

  implicit def pageEq: UnivEq[Page] = UnivEq.force

  val pages = NonEmptyVector[Page](
    ReqTable, CfgFields, CfgIssues, CfgReqTypes, CfgTags)

  val pageTitle: Page => String = {
    case ReqTable    => "Requirements Table"
    case CfgFields   => "Cfg: Fields"
    case CfgIssues   => "Cfg: Issues"
    case CfgReqTypes => "Cfg: Requirement Types"
    case CfgTags     => "Cfg: Tags"
  }

  def routes(r: Routines.ProjectSPA, cp: ClientProtocol, cd: ClientData) = RouterConfigDsl[Page].buildRule { dsl =>

    def reqTable =
      ui.reqtable.ReqTable.WIP(cd.project)

    def cfgIssues =
      ui.cfg.issues.CfgIssues.Props(cp, r.issueTypeCrud, r.reqTypeImpMod, r.fieldMandMod, cd, HideDead).component

    def cfgReqTypes =
      ui.cfg.CfgReqTypes.Props(cp, r.reqTypeCrud, cd, HideDead).component

    def cfgTags =
      ui.cfg.tags.CfgTags.Props(cp, r.tagCrud, cd, HideDead).component

    def cfgFields =
      ui.cfg.fields.CfgFields.Props(cp, r.fieldCrud, cd, HideDead).component

    import dsl._
    ( staticRoute("#tab",          ReqTable   ) ~> render(reqTable)
    | staticRoute("#cfg/fields",   CfgFields  ) ~> render(cfgFields)
    | staticRoute("#cfg/issues",   CfgIssues  ) ~> render(cfgIssues)
    | staticRoute("#cfg/reqtypes", CfgReqTypes) ~> render(cfgReqTypes)
    | staticRoute("#cfg/tags",     CfgTags    ) ~> render(cfgTags)
    )
  }
}

// ===================================================================================================================


object ReactExamples {

  def main(r: Routines.ProjectSPA) = IO[Unit] {
    example1(document getElementById "eg1")
//    manual()
    projectPage(r).unsafePerformIO()
  }

  // ===================================================================================================================

  sealed trait TmpPage
  case object Index extends TmpPage
  case class Real(page: ShipreqWip.Page) extends TmpPage

  val index = ReactComponentB[RouterCtl[TmpPage]]("Index")
    .render(ctl =>
      <.ul(
        ShipreqWip.pages.whole.map(p =>
          <.li(ctl.link(Real(p))(ShipreqWip pageTitle p))))
    ).build

  // val dnd        : Loc = register(location("#demo/dnd",     dragAndDropDemo))
  // <.li(router.link(ProjectPage.dnd        )("Demo: Drag 'n' Drop")))
  // def dragAndDropDemo = <.section(DragAndDrop_ol.demo, DragAndDrop_table.demo)

  def layout(ctl: RouterCtl[TmpPage], res: Resolution[TmpPage]): ReactElement =
    res.page match {
      case Index => res.render()
      case _ =>
        <.div(
          <.div(
            ^.textAlign.right,
            ^.paddingRight := "0.6ex",
            ^.marginBottom := "1em",
            ^.backgroundColor := "#ddd",
            ctl.link(Index)("← Back")),
          res.render())
    }

  def projectPage(r: Routines.ProjectSPA): IO[Unit] = {
    val cp = ClientProtocol.Lift
    ClientData.init(cp, r.projectInit, clientData => IO {

      val routerConfig = RouterConfigDsl[TmpPage].buildConfig { dsl =>
        import dsl._

        ( staticRoute(root, Index) ~> renderR(index(_))
        | ShipreqWip.routes(r, cp, clientData).pmap[TmpPage](Real){case Real(p) => p}
        | trimSlashes
        ).notFound(redirectToPage(Index)(Redirect.Replace))
          .renderWith(layout)
          .verify(Index, ShipreqWip.pages.whole.map(Real): _*)
      }

      ui.Style.addToDocument()
      val router = Router(BaseUrl.fromWindowOrigin / "wip", routerConfig)
      router() render document.getElementById("eg2")
    })
  }

  // ===================================================================================================================

  def example1(mountNode: Node) = {
    val ReactTest = ReactComponentB[String]("React test")
      .render(p => <.div("React: ", p))
      .build
    React.render(ReactTest("works."), mountNode)
  }

  // ===================================================================================================================

  object DragAndDrop_ol {

    case class Item(id: Int, name: String)
    implicit val itemEquivalence = scalaz.Equal.equalBy((_: Item).id)

    val RowComp = DND.Child.dndItemComponent[Item]((outerAttr, hnd, i) =>
      <.div(outerAttr,
        <.div(hnd, s"${i.id} | ${i.name}")))

    case class ParentState(items: Vector[Item], dnd: DND.Parent.PState[Item])

    val Component = ReactComponentB[Vector[Item]]("DragAndDrop")
      .getInitialState(p => ParentState(p, DND.Parent.initialState))
      .render(T => {
        // console.log(s"DND.State = ${T.state}")
        val itemsState = T.focusState(_.items)((a, b) => a.copy(items = b))
        val dndState = T.focusState(_.dnd)((a, b) => a.copy(dnd = b))

        def move(from: Item, to: Item) =
//          console.log(s"...Before = ${T.state}")
          itemsState.modStateIO(DND.move(from, to))
//          console.log(s"....After = ${T.state}")

        def renderItem(i: Item) =
          <.li(^.key := i.id)(RowComp(DND.Parent.cProps2(dndState, i, move)))

        <.div(
          <.h2("Drag and Drop (ol)"),
          <.ol(T.state.items.map(renderItem).toJsArray)

        )
      }).build

    def demo =
      Component(Vector(
        Item(10, "Ten")
        ,Item(20, "Two Zero")
        ,Item(30, "Firty")
        ,Item(40, "Thorty")
        ,Item(50, "Fipty")
      ))
  }

  object DragAndDrop_table {

    case class Item(id: Int, name: String)
    implicit val itemEquivalence = scalaz.Equal.equalBy((_: Item).id)

    val RowComp = DND.Child.dndItemComponent[Item]((outerAttr, hnd, i) =>
      <.tr(outerAttr,
        <.td(hnd),
        <.td(s"${i.id} | ${i.name}")))

    case class ParentState(items: Vector[Item], dnd: DND.Parent.PState[Item])

    val Component = ReactComponentB[Vector[Item]]("DragAndDrop")
      .getInitialState(p => ParentState(p, DND.Parent.initialState))
      .render(T => {
      // console.log(s"DND.State = ${T.state}")
      val itemsState = T.focusState(_.items)((a, b) => a.copy(items = b))
      val dndState = T.focusState(_.dnd)((a, b) => a.copy(dnd = b))

      def move(from: Item, to: Item) =
        //          console.log(s"...Before = ${T.state}")
        itemsState.modStateIO(DND.move(from, to))
        //          console.log(s"....After = ${T.state}")

      def renderItem(i: Item) =
        RowComp.set(key = (i.id: js.Any))(DND.Parent.cProps2(dndState, i, move))

      <.div(
        <.h2("Drag and Drop (table)"),
        <.table(
          <.tbody(
            T.state.items.map(renderItem).toReactNodeArray))

      )
    }).build

    def demo =
      Component(Vector(
        Item(10, "Ten")
        ,Item(20, "Two Zero")
        ,Item(30, "Firty")
        ,Item(40, "Thorty")
        ,Item(50, "Fipty")
      ))
  }
}
