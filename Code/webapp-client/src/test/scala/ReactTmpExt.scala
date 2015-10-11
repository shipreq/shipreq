package shipreq.webapp.client.test

import japgolly.scalajs.react._
import org.scalajs.dom.Node

object ReactTmpExt {

  implicit class ASDASD_1[P, S, B, N <: TopNode](val r: ReactComponentC[P, S, B, N]) extends AnyVal {
    def castM(c: ReactComponentM_[_]) = c.asInstanceOf[r.Mounted]
    def castU(c: ReactComponentU_) = c.asInstanceOf[r.Unmounted]

//    val x0 = ReactTestUtils.findRenderedComponentWithType(c, Table.Component.jsCtor)
//    val x = Table.Component.castM(x0)

  }

}
