package hahaa

import shipreq.webapp.client.ClientData
import shipreq.webapp.client.lib.{TableIoProps, TableIoArb}
import shipreq.webapp.client.protocol.{FailureIO, ClientProtocol}
import shipreq.webapp.base.protocol.Routines

import scala.scalajs.js
import org.scalajs.dom.{document, window, Node, console, alert}
import scala.scalajs.js.annotation.{JSExport, JSName}
import scalaz.effect.IO

object ReactExamples {

  def main(routines: Routines.ForCfgReqType) = IO[Unit] {
    example1(document getElementById "eg1")

    import shipreq.webapp.client._
    ClientData.init(routines.projectInit, clientData => IO {
      CfgReqType.Component(TableIoProps(TableIoArb(routines.reqCrud, clientData), false)) render document.getElementById("eg2")
      CfgIncmpType.Component(TableIoProps(TableIoArb(routines.incmpCrud, clientData), false)) render document.getElementById("eg3")
    }).unsafePerformIO()
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
}
