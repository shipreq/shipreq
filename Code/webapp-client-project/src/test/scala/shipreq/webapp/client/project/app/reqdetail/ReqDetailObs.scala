package shipreq.webapp.client.project.app.reqdetail

import org.scalajs.dom.html
import scala.util.Try
import shipreq.webapp.base.UiText
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.{Dead, FilterDead, Live, ShowDead}
import shipreq.webapp.client.base.test.TestState._
import shipreq.webapp.client.base.ui.semantic.Icon
import shipreq.webapp.client.project.widgets.high.DeletionFormObs
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
    private val root = $(">*")

    val headerRow = root(">div")

    val pubid = headerRow(">*", 1 of 3).innerText.replace(":", "").trim

    val titleDom = headerRow(">*", 2 of 3).asHtml.dom

    val filterDeadButton = headerRow(">*", 3 of 3)("button").domAs[html.Button]

    val filterDead = ShowDead <~ filterDeadButton.className.contains("red")

    val filterDeadLocked = filterDeadButton.disabled getOrElse false

    val table = root(">table")

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
      treeCells.map(_.collect0n(">div>div").mapZippers(StepRow))

    case class StepRow($: HtmlDomZipper) {
      private def ctrl(icon: Icon, icon2: Icon = null): html.Button = {
        val is  = (icon :: Option(icon2).toList).map(_.clsName.replace(' ', '.'))
        val sel = is.map(i => s"button:has(i.icon.$i)") mkString ","
        $(sel).domAs[html.Button]
      }

      val label: Option[String] =
        $.collect01("*[data-step-label]").asHtml.mapDoms(_.title)

      lazy val textContainer = $("*[data-step-text]").asHtml

      lazy val text = textContainer.innerText

      lazy val textEditor = textContainer("textarea").domAs[html.TextArea]

      lazy val del   = ctrl(UseCaseStepControls.IconDelete)
      lazy val rest  = ctrl(UseCaseStepControls.IconRestore)
      lazy val left  = ctrl(UseCaseStepControls.IconShiftLeft)
      lazy val right = ctrl(UseCaseStepControls.IconShiftRight)
      lazy val add   = ctrl(UseCaseStepControls.IconAdd)
    }

    def row(label: String): StepRow =
      stepRows.get(_.find(_.label.exists(_ ==* label))) getOrElse sys.error("Step row not found: " + label)

    val treeStepLabels: NAE[Vector[String]] =
      stepRows.map(_.flatMap(_.label.toVector))

    val stepLabels: Vector[String] =
      treeStepLabels.reduce(_ ++ _)

    private def getTailStepRow(rows: Vector[StepRow]): Option[StepRow] = {
      val tailStepRows = rows.toIterator.zipWithIndex.filter(_._1.$.dom.hasAttribute("data-tail-step-row")).toList
      tailStepRows match {
        case Nil => None
        case (row, i) :: Nil =>
          assert(i == rows.indices.last, "Tail step row isn't in last position!")
          Some(row)
        case _ =>
          sys error "Multiple tail step rows found!"
      }
    }

    def tailStepRowAC = getTailStepRow(stepRows.alt)
    def tailStepRowEC = getTailStepRow(stepRows.exception)
  }

  val editables =
    $.editables0n.doms

  val mode: Mode =
    if (errorRoot.isDefined)
      Mode.Error
    else if (deletionForm.isDefined)
      Mode.Delete
    else
      Mode.Details

  override def toString = s"ReqDetailObs($mode)"
}
