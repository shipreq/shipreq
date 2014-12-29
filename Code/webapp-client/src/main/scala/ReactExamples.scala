package hahaa

import org.scalajs.dom._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.DND
import scalaz.syntax.bind.ToBindOps
import scala.scalajs.js
import scalaz.effect.IO
import scalaz.std.AllInstances._
import shipreq.webapp.base.protocol.Routines
import japgolly.scalajs.react._, vdom.all._, ScalazReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router._
import shipreq.webapp.client.app.ui._

object ReactExamples {

  def main(r: Routines.ForCfgReqType) = IO[Unit] {
    example1(document getElementById "eg1")
//    manual()
    projectPage(r).unsafePerformIO()
  }

  // ===================================================================================================================

  def projectPage(r: Routines.ForCfgReqType): IO[Unit] = {
    import shipreq.webapp.client._
    val cp = ClientProtocol.Lift
    ClientData.init(cp, r.projectInit, clientData => IO {

      object ProjectPage extends RoutingRules {
        val root       : Loc = register(rootLocation(index))
        val cfgIncmp   : Loc = register(location("#cfg/incmp",    cfgIncmpR))
        val cfgReqTypes: Loc = register(location("#cfg/reqtypes", cfgReqTypesR))
        val cfgTags     = path("#cfg/tags",     addBack(root, cfgTagsR))
        val dnd         = path("#demo/dnd",     addBack(root, dndR))

        private def index: Renderer = router => {
          val c = ReactComponentB[Unit]("Index")
            .render(_ =>
              ul(
                li(router.link(ProjectPage.cfgIncmp   )("Cfg: Incompletions")),
            li(router.link(ProjectPage.cfgReqTypes)("Cfg: Requirement Types")),
            li(router.link(ProjectPage.cfgTags)("Cfg: Tags")),
            li(router.link(ProjectPage.dnd)("Demo: Drag 'n' Drop")))
            ).buildU
          c()
        }

        private def cfgIncmpR =
        cfg.CfgIncompletions.comp(cfg.CfgIncompletions.Props(cp, r.incmpCrud, r.reqTypeImpMod, clientData, false))

        private def cfgReqTypesR =
        cfg.CfgReqTypes.Props(cp, r.reqTypeCrud, clientData, false).component

      def cfgTagsR: Renderer[ProjectPage] = _ =>
        cfg.tags.CfgTags.Props(cp, r.tagCrud, clientData, false).component

      def dndR: Renderer[ProjectPage] = _ =>
        DragAndDrop.demo

        register(removeTrailingSlashes)

        override protected val notFound = redirect(root, Redirect.Replace)

        override protected def interceptRender(i: InterceptionR): ReactElement =
          if (i.loc == root)
            i.element
          else
            div(
              div(backgroundColor := "#ddd", i.router.link(root)("Back", cls := "back")),
              i.element)
      }

      val c = ProjectPage.router(BaseUrl.fromWindowOrigin / "wip", Router.consoleLogger)
      c() render document.getElementById("eg2")
    })
  }

  // ===================================================================================================================

  def example1(mountNode: Node) = {
    val HelloMessage = ReactComponentB[String]("HelloMessage")
      .render(name => div("Hello ", name))
      .build
    React.render(HelloMessage("John"), mountNode)
  }
  }

  // ===================================================================================================================

  object DragAndDrop {

    case class Item(id: Int, name: String)
    implicit val itemEquivalence = scalaz.Equal.equalBy((_: Item).id)

    val RowComp = DND.Child.dndItemComponent[Item](
      (i, hnd) => div(hnd, s"${i.id} | ${i.name}"))

    case class ParentState(items: List[Item], dnd: DND.Parent.PState[Item], i: Int)

    val Component = ReactComponentB[List[Item]]("DragAndDrop")
      .getInitialState(p => ParentState(p, DND.Parent.initialState, 0))
      .render(T => {
        // console.log(s"DND.State = ${T.state}")
        val itemsState = T.focusState(_.items)((a, b) => a.copy(items = b))
        val dndState = T.focusState(_.dnd)((a, b) => a.copy(dnd = b))

        def move(from: Item, to: Item) = IO {
//          console.log(s"...Before = ${T.state}")
          itemsState.modState(DND.move(from, to))
//          console.log(s"....After = ${T.state}")
        }

        def renderItem(i: Item) =
          li(key := i.id)(RowComp(DND.Parent.cProps2(dndState, i, move)))

        div(
          h1("Drag and Drop"),
          ol(T.state.items.map(renderItem).toJsArray)

        )
      }).build

    def demo =
      DragAndDrop.Component(List(
        DragAndDrop.Item(10, "Ten")
        ,DragAndDrop.Item(20, "Two Zero")
        ,DragAndDrop.Item(30, "Firty")
        ,DragAndDrop.Item(40, "Thorty")
        ,DragAndDrop.Item(50, "Fipty")
      ))
}
