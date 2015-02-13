package hahaa

import org.scalajs.dom._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.DND
import scalaz.syntax.bind.ToBindOps
import scala.scalajs.js
import scalaz.effect.IO
import scalaz.std.AllInstances._
import shipreq.webapp.base.protocol.Routines
import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router._
import shipreq.webapp.client.app.ui._

object ReactExamples {

  def main(r: Routines.ProjectSPA) = IO[Unit] {
    example1(document getElementById "eg1")
//    manual()
    projectPage(r).unsafePerformIO()
  }

  // ===================================================================================================================

  def projectPage(r: Routines.ProjectSPA): IO[Unit] = {
    import shipreq.webapp.client._
    val cp = ClientProtocol.Lift
    ClientData.init(cp, r.projectInit, clientData => IO {

      object ProjectPage extends RoutingRules {
        val root       : Loc = register(rootLocation(index))
        val tab        : Loc = register(location("#tab",          tabR))
        val cfgFields  : Loc = register(location("#cfg/fields",   cfgFieldsR))
        val cfgIssues  : Loc = register(location("#cfg/issues",   cfgIssuesR))
        val cfgReqTypes: Loc = register(location("#cfg/reqtypes", cfgReqTypesR))
        val cfgTags    : Loc = register(location("#cfg/tags",     cfgTagsR))
        val dnd        : Loc = register(location("#demo/dnd",     dragAndDropDemo))

        private def index: Renderer = router => {
          val c = ReactComponentB[Unit]("Index")
            .render(_ =>
              <.ul(
                <.li(router.link(ProjectPage.tab        )("Requirements Table")),
                <.li(router.link(ProjectPage.cfgFields  )("Cfg: Fields")),
                <.li(router.link(ProjectPage.cfgIssues  )("Cfg: Issues")),
                <.li(router.link(ProjectPage.cfgReqTypes)("Cfg: Requirement Types")),
                <.li(router.link(ProjectPage.cfgTags    )("Cfg: Tags")),
                <.li(router.link(ProjectPage.dnd        )("Demo: Drag 'n' Drop")))
            ).buildU
          c()
        }

        private def tabR =
          reqtable.ReqTable.WIP(clientData.project)

        private def cfgIssuesR =
          cfg.issues.CfgIssues.Props(cp, r.issueTypeCrud, r.reqTypeImpMod, r.fieldMandMod, clientData, false).component

        private def cfgReqTypesR =
          cfg.CfgReqTypes.Props(cp, r.reqTypeCrud, clientData, false).component

        private def cfgTagsR =
          cfg.tags.CfgTags.Props(cp, r.tagCrud, clientData, false).component

        private def cfgFieldsR =
          cfg.fields.CfgFields.Props(cp, r.fieldCrud, clientData, false).component

        def dragAndDropDemo = <.section(DragAndDrop_ol.demo, DragAndDrop_table.demo)

        register(removeTrailingSlashes)

        override protected val notFound = redirect(root, Redirect.Replace)

        override protected def interceptRender(i: InterceptionR): ReactElement =
          if (i.loc == root)
            i.element
          else
            <.div(
              <.div(^.backgroundColor := "#ddd", i.router.link(root)("Back", ^.cls := "back")),
              i.element)
      }

      val c = ProjectPage.router(BaseUrl.fromWindowOrigin / "wip")
      c() render document.getElementById("eg2")
    })
  }

  // ===================================================================================================================

  def example1(mountNode: Node) = {
    val HelloMessage = ReactComponentB[String]("HelloMessage")
      .render(name => <.div("Hello ", name))
      .build
    React.render(HelloMessage("John"), mountNode)
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
