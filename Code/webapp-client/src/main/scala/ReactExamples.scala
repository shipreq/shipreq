package hahaa

import shipreq.webapp.client.ClientData
import shipreq.webapp.client.protocol.{FailureIO, ClientProtocol}
import shipreq.webapp.shared.protocol.Routines

import scala.scalajs.js
import org.scalajs.dom.{document, window, Node, console, alert}
import scala.scalajs.js.annotation.{JSExport, JSName}
import scalaz.effect.IO

object ReactExamples {

  def main(routines: Routines.ForCfgReqType) = IO[Unit] {
    example1(document getElementById "eg1")
    //    example2(document getElementById "eg2")
    cfgReqTypes(routines, document getElementById "eg2")
  }

  // ===================================================================================================================

  import japgolly.scalajs.react._
  import vdom.ReactVDom._
  import all._

  def example1(mountNode: Node) = {

    val HelloMessage = ReactComponentB[String]("HelloMessage")
      .render(name => div("Hello ", name))
      .build

    React.renderComponent(HelloMessage("John"), mountNode)
  }

  // ===================================================================================================================

  def cfgReqTypes(routines: Routines.ForCfgReqType, mountNode: Node) = {
    import shipreq.webapp.client.CfgReqType._

    ClientData.init(routines.projectInit, clientData => IO {
      Component(Props((routines.reqCrud, clientData), false)) render mountNode
    }).unsafePerformIO()
  }
}
