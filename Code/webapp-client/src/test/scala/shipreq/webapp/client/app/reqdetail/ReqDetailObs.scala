package shipreq.webapp.client.app.reqdetail

import shipreq.webapp.base.UiText
import shipreq.base.util.ScalaExt._
import shipreq.webapp.client.test._
import DomZipper.Implicits._
import ReqDetailTestDsl.Mode

object ReqDetailObs {

  case class NAE[A](normal: A, alt: A, exception: A) {
    def map[B](f: A => B): NAE[B] =
      NAE(f(normal), f(alt), f(exception))

    def reduce[B](f: (A, A) => A): A =
      f(f(normal, alt), exception)
  }

  import UiText.FieldNames._
  val TreeNames = NAE(useCaseStepTreeN, useCaseStepTreeA, useCaseStepTreeE)
}

final class ReqDetailObs($: DomZipper) {

  private val errorRoot = $.down("h5")(DomZipper.ReturnOption)

  object error {
    val reason = errorRoot.get.innerText
  }

  object generic {
    val headerRow = $.down(">div")

    val pubid = headerRow.down(">div", 1 of 2).innerText.replace(":", "").trim

    val table = $.down(">table")

    val fields =
      table.down(">tbody").collect1(">tr")
        .map(z => z.down(">th").innerText -> z.down(">td"))
        .toMap
  }

  object uc {
    import generic._

    val treeCells = ReqDetailObs.TreeNames.map(fields)

    val treeStepTitles = treeCells.map(_.collect0(s"*[data-step-label]").asHtml.mapDom(_.title))

    val stepTitles: Vector[String] = treeStepTitles.reduce(_ ++ _)
  }

  val mode: Mode =
    if (errorRoot.isDefined)
      Mode.Error
    else if (generic.pubid.startsWith("UC-"))
      Mode.UC
    else
      Mode.GR

//    (Try(ok), Try(error)) match {
//      case (Success(_), Failure(_)) => true
//      case (Failure(_), Success(_)) => false
//      case (Success(_), Success(_)) => sys error "Ok or Error, which one? Not both."
//      case (Failure(a), Failure(b)) => sys error s"Ok & Error both failed.\nOK - $a\nKO - $b"
//    }
}
