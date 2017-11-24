package shipreq.webapp.client.project.app.reqdetail

import japgolly.univeq._
import org.scalajs.dom.html
import shipreq.base.util.LeftRight
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{Dead, Live, ShowDead}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.client.project.app.TestMarker
import shipreq.webapp.client.project.feature.deletion.{DeletionFormObs, RestorationFormObs}
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

  private val errorRoot = $.failToOption(".ui.error.message")

  object error {
    val reason = errorRoot.get.innerText
  }

  val deletionForm = DeletionFormObs.option($)
  val restorationForm = RestorationFormObs.option($)

  object generic {
    private val root = $(">*")

    val headerRow = root(">div")

    val pubid = headerRow(">*", 1 of 3).innerText.replace(":", "").trim

    val titleDom = headerRow(">*", 2 of 3).asHtml.dom

    val filterDeadButton = headerRow(">*", 3 of 3)("button").domAs[html.Button]

    val filterDead = ShowDead when filterDeadButton.className.contains("red")

    val filterDeadLocked = filterDeadButton.disabled

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
      private def ctrl(icon: Icon, icon2: Icon = null): Option[html.Button] = {
        val is  = (icon :: Option(icon2).toList).map(_.clsName.replace(' ', '.'))
        val sel = is.map(i => s"button:has(i.icon.$i)") mkString ","
        $.collect01(sel).as[html.Button].doms
      }

      val isTailStepRow: Boolean =
        $.dom.hasAttribute(TestMarker.useCaseTailStep.name)

      val label: Option[String] =
        $.collect01(s"*[${TestMarker.useCaseStepLabel.name}]").asHtml.mapDoms(_.title)

      lazy val textContainer = $(s"*[${TestMarker.useCaseStepText.name}]").asHtml

      lazy val text = textContainer.innerText

      lazy val textEditor = textContainer("textarea").domAs[html.TextArea]

      val del   = ctrl(UseCaseStepControls.IconDelete)
      val rest  = ctrl(UseCaseStepControls.IconRestore)
      val left  = ctrl(UseCaseStepControls.IconShift(LeftRight.Left))
      val right = ctrl(UseCaseStepControls.IconShift(LeftRight.Right))
      val add   = ctrl(UseCaseStepControls.IconAdd)

      val buttons = List(del, rest, left, right, add).flatMap(_.toList)
    }

    def row(label: String): StepRow =
      stepRows.get(_.find(_.label.exists(_ ==* label))) getOrElse sys.error("Step row not found: " + label)

    val treeStepLabels: NAE[Vector[String]] =
      stepRows.map(_.flatMap(_.label.toVector))

    val stepLabels: Vector[String] =
      treeStepLabels.reduce(_ ++ _)

    val allRows: Vector[StepRow] =
      stepRows.reduce(_ ++ _)

    private def getTailStepRow(rows: Vector[StepRow]): Option[StepRow] = {
      val tailStepRows = rows.toIterator.zipWithIndex.filter(_._1.isTailStepRow).toList
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
    else if (restorationForm.isDefined)
      Mode.Restore
    else
      Mode.Details

  override def toString = s"ReqDetailObs($mode)"
}
