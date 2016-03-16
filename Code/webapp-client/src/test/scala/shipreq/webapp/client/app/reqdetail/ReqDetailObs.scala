package shipreq.webapp.client.app.reqdetail

import org.scalajs.dom.html
import shipreq.webapp.base.UiText
import shipreq.base.util.univEqOps
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.{Dead, Live}
import shipreq.webapp.client.data.{ShowDead, FilterDead}
import shipreq.webapp.client.test._
import DomZipper.Implicits._
import ReqDetailTestDsl.Mode
import ReqDetailObs.NAE

object ReqDetailObs {

  case class NAE[A](normal: A, alt: A, exception: A) {
    def map[B](f: A => B): NAE[B] =
      NAE(f(normal), f(alt), f(exception))

    def reduce[B](f: (A, A) => A): A =
      f(f(normal, alt), exception)

    def get[B](f: A => Option[B]): Option[B] =
      f(normal) orElse f(alt) orElse f(exception)
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

    val filterDeadInput = $.down(">label input").domAs[html.Input]

    val filterDead = ShowDead <~ filterDeadInput.checked

    val filterDeadLocked = filterDeadInput.disabled

    val fields: Map[String, DomZipper] =
      table.down(">tbody").collect1(">tr")
        .map(z => z.down(">th").innerText -> z.down(">td"))
        .toMap

    val lifeRow = fields(UiText.Life.field)

    val live: Live = {
      val t = lifeRow.innerText
      if (t startsWith UiText.Life.live)
        Live
      else if (t startsWith UiText.Life.dead)
        Dead
      else
        sys error s"Expected live or dead, got: $t"
    }
  }

  object uc {
    import generic._

    val treeCells = ReqDetailObs.TreeNames.map(fields)

    val stepRows: NAE[Vector[StepRow]] =
      treeCells.map(_.collect1(">div").map(StepRow))

    case class StepRow($: DomZipper) {
      private def ctrl(label: String, label2: String = null): html.Button = {
        val ls  = label :: Option(label2).toList
        val sel = ls.map(l => s"button:contains('$l')") mkString ","
        $.down(sel).domAs[html.Button]
      }

      val title: Option[String] =
        $.collect0(s"*[data-step-label]").asHtml.mapDom(_.title).headOption

      lazy val del   = ctrl("-")
      lazy val left  = ctrl("«", "↓")
      lazy val right = ctrl("»", "↑")
      lazy val add   = ctrl("+")
    }

    def row(label: String): StepRow =
      stepRows.get(_.find(_.title.exists(_ ==* label))) getOrElse sys.error("Step row not found: " + label)

    val treeStepTitles: NAE[Vector[String]] =
      stepRows.map(_.flatMap(_.title.toVector))

    val stepTitles: Vector[String] =
      treeStepTitles.reduce(_ ++ _)

    def tailStepRowAC = stepRows.alt.last
    def tailStepRowEC = stepRows.exception.last
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

  override def toString = s"ReqDetailObs($mode)"
}
