package hahaa

import org.scalajs.dom._
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
    import shipreq.webapp.client.app.ui._
    ClientData.init(r.projectInit, clientData => IO {

      object ProjectPage extends RoutingRules {
        val root       : Loc = register(rootLocation(index))
        val cfgIncmp   : Loc = register(location("#cfg/incmp",    cfgIncmpR))
        val cfgReqTypes: Loc = register(location("#cfg/reqtypes", cfgReqTypesR))

        private def index: Renderer = router => {
          val c = ReactComponentB[Unit]("Index")
            .render(_ =>
              ul(
                li(router.link(ProjectPage.cfgIncmp   )("Cfg: Incompletions")),
                li(router.link(ProjectPage.cfgReqTypes)("Cfg: Requirement Types")))
            ).buildU
          c()
        }

        private def cfgIncmpR =
          CfgIncompletions.comp(CfgIncompletions.Props(r.incmpCrud, r.reqTypeImpMod, clientData, false))

        private def cfgReqTypesR =
          CfgReqTypes.Props(r.reqTypeCrud, clientData, false).component

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
