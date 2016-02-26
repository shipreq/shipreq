package shipreq.webapp.client.app.reqdetail

import scala.util.{Failure, Success, Try}
import shipreq.webapp.client.test._
import DomZipper.Implicits._

final class ReqDetailObs($: DomZipper) {

  private val errorRoot = $.down("h5")(DomZipper.ReturnOption)

  object error {
    val reason = errorRoot.get.innerText
  }

  object ok {
    val table = $.down(">table")
  }

  val isOk: Boolean =
    errorRoot.isEmpty
//    (Try(ok), Try(error)) match {
//      case (Success(_), Failure(_)) => true
//      case (Failure(_), Success(_)) => false
//      case (Success(_), Success(_)) => sys error "Ok or Error, which one? Not both."
//      case (Failure(a), Failure(b)) => sys error s"Ok & Error both failed.\nOK - $a\nKO - $b"
//    }
}
