package hahaa

import org.scalajs.dom._
import shipreq.webapp.client.protocol.ClientProtocol
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
      }

      def index: Renderer[ProjectPage] = router => {
        val c = ReactComponentB[Unit]("Index")
          .render(_ =>
          ul(
            li(router.link(ProjectPage.cfgIncmp)("Cfg: Incompletions")),
            li(router.link(ProjectPage.cfgReqTypes)("Cfg: Requirement Types")))
          ).buildU
        c()
      }

      def cfgIncmpR: Renderer[ProjectPage] = _ =>
        CfgIncompletions.comp(CfgIncompletions.Props(cp, r.incmpCrud, r.reqTypeImpMod, clientData, false))

      def cfgReqTypesR: Renderer[ProjectPage] = _ =>
        CfgReqTypes.Props(cp, r.reqTypeCrud, clientData, false).component

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

}
