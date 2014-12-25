package hahaa

import org.scalajs.dom._
import shipreq.webapp.client.protocol.ClientProtocol
import shipreq.webapp.client.util.DND
import scalaz.syntax.bind.ToBindOps
import scala.scalajs.js
import scalaz.effect.IO
import scalaz.std.AllInstances._
import shipreq.webapp.base.protocol.Routines
import shipreq.webapp.client.util.route._
import japgolly.scalajs.react._, vdom.ReactVDom._, all._, ScalazReact._, experiment._
import shipreq.webapp.client.app.ui._

object ReactExamples {

  def main(r: Routines.ForCfgReqType) = IO[Unit] {
    example1(document getElementById "eg1")
//    manual()
    projectPage(r).unsafePerformIO()
  }

  def addBack[P](root: Root[P], inner: Renderer[P]): Renderer[P] = router => {
    val c = ReactComponentB[Unit]("Outer")
      .render(_ =>
      div(
        div(backgroundColor := "#ddd", router.link(root)("Back")),
        inner(router))
      ).buildU
    c()
  }

  // ===================================================================================================================

  def projectPage(r: Routines.ForCfgReqType): IO[Unit] = {
    import shipreq.webapp.client._
    val cp = ClientProtocol.Lift
    ClientData.init(cp, r.projectInit, clientData => IO {

      sealed trait ProjectPage
      object ProjectPage extends Page[ProjectPage] {
        val root = Root(index)
        val cfgIncmp    = path("#cfg/incmp",    addBack(root, cfgIncmpR))
        val cfgReqTypes = path("#cfg/reqtypes", addBack(root, cfgReqTypesR))
        val cfgTags     = path("#cfg/tags",     addBack(root, cfgTagsR))
        val dnd         = path("#demo/dnd",     addBack(root, dndR))
      }

      def index: Renderer[ProjectPage] = router => {
        val c = ReactComponentB[Unit]("Index")
          .render(_ =>
          ul(
            li(router.link(ProjectPage.cfgIncmp)("Cfg: Incompletions")),
            li(router.link(ProjectPage.cfgReqTypes)("Cfg: Requirement Types")),
            li(router.link(ProjectPage.cfgTags)("Cfg: Tags")),
            li(router.link(ProjectPage.dnd)("Demo: Drag 'n' Drop")))
          ).buildU
        c()
      }

      def cfgIncmpR: Renderer[ProjectPage] = _ =>
        cfg.CfgIncompletions.comp(cfg.CfgIncompletions.Props(cp, r.incmpCrud, r.reqTypeImpMod, clientData, false))

      def cfgReqTypesR: Renderer[ProjectPage] = _ =>
        cfg.CfgReqTypes.Props(cp, r.reqTypeCrud, clientData, false).component

      def cfgTagsR: Renderer[ProjectPage] = _ =>
        cfg.tags.CfgTags.Props(cp, r.tagCrud, clientData, false).component

      def dndR: Renderer[ProjectPage] = _ =>
        DragAndDrop.demo

      val c = Router.component(BaseUrl("/wip"), ProjectPage)
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

  def manual() = {
    val tgt = document.getElementById("eg2")

    sealed trait MyPage
    object MyPage extends Page[MyPage] {
      val root = Root(RootC)
      val f2 = path("#f2", addBack(root, Route2C))
      val f3 = path("#f3", addBack(root, Route3C))
    }

    def RootC: Renderer[MyPage] = router => {
      val c = ReactComponentB[Unit]("RootC")
        .render(_ =>
        div(
          h2("Top Level. Top Secret."),
          div(router.link(MyPage.f2)("F222222222222222222222222")),
          div(router.link(MyPage.f3)("F333333333333333333333333")))
        )
        .configure(LogLifecycle.short)
        .buildU
      c()
    }

    def Route2C: Renderer[MyPage] = router => {
      val c = ReactComponentB[Unit]("F2")
        .render(_ => div(h3("Cool.")))
        .configure(LogLifecycle.short)
        .buildU
      c()
    }

    def Route3C: Renderer[MyPage] = router => {
      val c = ReactComponentB[String]("F3")
        .render(p => div(h3("Hello ", p)))
        .configure(LogLifecycle.short)
        .build
      c("hehe cool")
    }

    val c = Router.component(BaseUrl("/wip"), MyPage)
    React.render(c(), tgt)
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

}
