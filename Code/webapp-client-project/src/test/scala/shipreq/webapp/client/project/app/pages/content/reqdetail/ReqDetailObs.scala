package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.univeq._
import org.scalajs.dom.html
import shipreq.base.util.LeftRight
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{Dead, Live, ShowDead, StaticField}
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.client.project.app.ProjectSpaTestDsl.NavObs
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
  val TreeNames = NAE(useCaseStepTreeN, useCaseStepTreeA, StaticField.ExceptionStepTree.name)
}

final class ReqDetailObs($: DomZipperJs, val nav: NavObs) {

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

    private val title = headerRow(">*", 2 of 3)
    val titleDom = title.domAsHtml
    val titleEditor = title.collect01("textarea").domsAs[html.TextArea]

    val filterDeadButton = headerRow(">*", 3 of 3)("button").domAs[html.Button]

    val filterDead = ShowDead when filterDeadButton.className.contains("red")

    val filterDeadLocked = filterDeadButton.disabled

    val table = root(">table")

    private val rows = table(">tbody").collect1n(">tr")

    val fieldsInOrder: Vector[String] =
      rows.map(_(">th").innerText)

    val fields: Map[String, Field] =
      rows.map(z => z(">th").innerText -> Field(z(">td"))).toMap

    def field(name: String): Field =
      fields.getOrElse(name, throw new RuntimeException("Field not found: " + name))

    case class Field(private[ReqDetailObs] val $: DomZipperJs) {
      val dom       = $.dom
      val innerText = $.innerText
      val editor    = $.collect01("textarea").domsAs[html.TextArea]
    }

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
      lifeRow.$.collect01("button").domsAs[html.Button]
  }

  object uc {
    import generic._

    private val treeCells = ReqDetailObs.TreeNames.map(fields(_).$)

    val stepRows: NAE[Vector[StepRow]] =
      treeCells.map(_.collect0n(">div>div").map(StepRow))

    case class StepRow($: DomZipperJs) {
      private def ctrl(icon: Icon, icon2: Icon = null): Option[html.Button] = {
        val is  = (icon :: Option(icon2).toList).map(_.clsName.replace(' ', '.'))
        val sel = is.map(i => s"button:has(i.icon.$i)") mkString ","
        $.collect01(sel).domsAs[html.Button]
      }

      val isTailStepRow: Boolean =
        $.dom.hasAttribute(TestMarker.useCaseTailStep.name)

      val label: Option[String] =
        $.collect01(s"*[${TestMarker.useCaseStepLabel.name}]").domsAsHtml.map(_.title)

      // tail steps just have buttons and nothing else
      val textContainer = $.collect01(TestMarker.useCaseStepText.cssSel).zippers
      val textEditor    = textContainer.flatMap(_.collect01("textarea").domsAs[html.TextArea])

      val isEditorOpen = textEditor.isDefined
      def isEditorClosed = !isEditorOpen

      val errorMsg: Option[String] =
        $.collect01(".ui.label.red").innerTexts

      lazy val text = {
        val t: String = textContainer.fold("")(_.innerText)
        t.indexOf("alt-left to unindent,") match {
          case -1 => t
          case i  => t.take(i)
        }
      }

      val del   = ctrl(UseCaseStepControls.IconDelete)
      val rest  = ctrl(UseCaseStepControls.IconRestore)
      val left  = ctrl(UseCaseStepControls.IconShift(LeftRight.Left))
      val right = ctrl(UseCaseStepControls.IconShift(LeftRight.Right))
      val add   = ctrl(UseCaseStepControls.IconAdd)

      private def can(b: Option[html.Button]): Boolean =
        b.exists(!_.disabled)

      val canDel   = can(del)
      val canRest  = can(rest)
      val canLeft  = can(left)
      val canRight = can(right)
      val canAdd   = can(add)

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
