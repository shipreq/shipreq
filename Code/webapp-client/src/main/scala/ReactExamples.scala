package hahaa

import org.scalajs.dom._
import scala.scalajs.js
//import scalaz._, Scalaz._
import scalaz.\/
import scalaz.effect.IO
import scalaz.std.option._
import scalaz.syntax.std.option._
import shipreq.webapp.base.protocol.Routines
import shipreq.webapp.client.lib._
import shipreq.webapp.client.ui._
import shipreq.webapp.client.util.route._

object ReactExamples {

  def main(r: Routines.ForCfgReqType) = IO[Unit] {
    example1(document getElementById "eg1")

    manual()

    import shipreq.webapp.client._
    ClientData.init(r.projectInit, clientData => IO {


//      CfgReqTypes.comp(TableIoProps(r.reqTypeCrud, clientData, false)) render document.getElementById("eg2")
//      CfgIncompletions.comp(CfgIncompletions.Props(r.incmpCrud, r.reqTypeImpMod, clientData, false)) render document.getElementById("eg3")
    }).unsafePerformIO()
  }

    // ===================================================================================================================

  import japgolly.scalajs.react._, vdom.ReactVDom._, all._, ScalazReact._

  def manual() = {
    val tgt = document.getElementById("eg2")

    sealed trait MyPage
    object MyPage extends Page[MyPage] {
      val root = Root(RootC)
      val f2 = path("#f2", addBack(Route2C))
      val f3 = path("#f3", addBack(Route3C))
    }

    def addBack(inner: Renderer[MyPage]): Renderer[MyPage] = router => {
      val c = ReactComponentB[Unit]("Outer")
        .render(_ =>
        div(
          div(backgroundColor := "#ddd", router.link(MyPage.root)("Back")),
          inner(router))
      ).buildU
      c()
    }

    def RootC: Renderer[MyPage] = router => {
      val c = ReactComponentB[Unit]("RootC")
        .render(_ =>
        div(
          h2("Top Level. Top Secret."),
          div(router.link(MyPage.f2)("F222222222222222222222222")),
          div(router.link(MyPage.f3)("F333333333333333333333333")))
        ).buildU
      c()
    }

    def Route2C: Renderer[MyPage] = router => {
      val c = ReactComponentB[Unit]("F2")
        .render(_ => div(h3("Cool.")))
        .buildU
      c()
    }

    def Route3C: Renderer[MyPage] = router => {
      val c = ReactComponentB[String]("F3")
        .render(p => div(h3("Hello ", p)))
        .build
      c("hehe cool")
    }

    val c = Router.component(BaseUrl("/wip"), MyPage)
    React.renderComponent(c(), tgt)
  }

  def example1(mountNode: Node) = {

    val HelloMessage = ReactComponentB[String]("HelloMessage")
      .render(name => div("Hello ", name))
      .build

    React.renderComponent(HelloMessage("John"), mountNode)
  }
}
