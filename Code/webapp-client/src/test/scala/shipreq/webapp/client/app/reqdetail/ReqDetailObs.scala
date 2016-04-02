package shipreq.webapp.client.app.reqdetail

import org.scalajs.dom.html
import scala.util.Try
import shipreq.webapp.base.UiText
import shipreq.base.util.univEqOps
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.{Dead, Live}
import shipreq.webapp.client.data.{ShowDead, FilterDead}
import shipreq.webapp.client.widgets.high.DeletionFormObs
import shipreq.webapp.client.test._
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

final class ReqDetailObs($: HtmlDomZipper) {

  private val errorRoot = $.failToOption("h5")

  object error {
    val reason = errorRoot.get.innerText
  }

  val deletionForm = DeletionFormObs.option($)

  object generic {
    val headerRow = $(">div")

    val pubid = headerRow(">*", 1 of 2).innerText.replace(":", "").trim

    val titleDom = headerRow(">*", 2 of 2).asHtml.dom

    val table = $(">table")

    val filterDeadInput = $(">label input").domAs[html.Input]

    val filterDead = ShowDead <~ filterDeadInput.checked

    val filterDeadLocked = filterDeadInput.disabled getOrElse false

    val fields: Map[String, HtmlDomZipperAt[html.TableCell]] =
      table(">tbody").collect1n(">tr")
        .mapZippers(z => z(">th").innerText -> z(">td").as[html.TableCell])
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

    val lifeChangeButton: Option[html.Button] =
      lifeRow.collect01("button").as[html.Button].doms
  }

  object uc {
    import generic._

    val treeCells = ReqDetailObs.TreeNames.map(fields)

    val stepRows: NAE[Vector[StepRow]] =
      treeCells.map(_.collect1n(">div").mapZippers(StepRow))

    case class StepRow($: HtmlDomZipper) {
      private def ctrl(label: String, label2: String = null): html.Button = {
        val ls  = label :: Option(label2).toList
        val sel = ls.map(l => s"button:contains('$l')") mkString ","
        $(sel).domAs[html.Button]
      }

      val title: Option[String] =
        $.collect01(s"*[data-step-label]").asHtml.mapDoms(_.title)

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

  val editables =
    $.editables0n.doms.filterNot(_ == Try(generic.filterDeadInput).getOrElse(null))

  val mode: Mode =
    if (errorRoot.isDefined)
      Mode.Error
    else if (deletionForm.isDefined)
      Mode.Delete
    else
      Mode.Details

  override def toString = s"ReqDetailObs($mode)"
}
