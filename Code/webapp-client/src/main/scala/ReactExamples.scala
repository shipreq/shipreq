package hahaa

import org.scalajs.dom._
import scalaz.effect.IO
import shipreq.webapp.base.protocol.Routines
import shipreq.webapp.client.lib.{TableIoProps, TableIoArb}
import shipreq.webapp.client.ui._

object ReactExamples {

  def main(routines: Routines.ForCfgReqType) = IO[Unit] {
    example1(document getElementById "eg1")

    import shipreq.webapp.client._
    ClientData.init(routines.projectInit, clientData => IO {
      CfgReqType.Component(TableIoProps(TableIoArb(routines.reqCrud, clientData), false)) render document.getElementById("eg2")
      CfgIncmpType.Component(TableIoProps(TableIoArb(routines.incmpCrud, clientData), false)) render document.getElementById("eg3")
      CfgReqType2.Component(TableIoArb(routines.reqImpReq, clientData)) render document.getElementById("eg4")
    }).unsafePerformIO()
  }

  // ===================================================================================================================

  import japgolly.scalajs.react._, vdom.ReactVDom._, all._

  def example1(mountNode: Node) = {

    val HelloMessage = ReactComponentB[String]("HelloMessage")
      .render(name => div("Hello ", name))
      .build

    React.renderComponent(HelloMessage("John"), mountNode)
  }
}
