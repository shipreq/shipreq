/*
package shipreq.webapp.client.project.app.reqtable

import monocle.Optional
import scalacss.ScalaCssReact._
import japgolly.microlibs.nonempty._
import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.Memo
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.{PlainText, ProjectText}
import shipreq.webapp.client.base.data.Plain
import shipreq.webapp.client.project.app.Style.{reqtable => *}
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.widgets.ProjectWidgets
import ColumnRenderer._

final class ColumnRenderer(val column: Column,
                           val view  : Row => View)

object ColumnRenderer {
  implicit val reusabilityCR: Reusability[ColumnRenderer] =
    Reusability.byRef // it's memo'ised

  implicit val reusabilityCRs: Reusability[NonEmptyVector[ColumnRenderer]] =
    Reusability.byRef || reusabilityNonEmptyVector

  sealed abstract class View {
    def render: VdomElement
  }

  // This could be lazy but 99% of the time, the view is displayed. May as well reduce the overhead of the 99% and have
  // this be strict.
  case class Render(render: VdomElement) extends View

  case object `N/A` extends View {
    val tag: VdomTag =
      <.span(*.`N/A`, "–")

    override val render: VdomElement =
      tag
  }

  val emptyTag: VdomTag     = <.span
  val empty   : VdomElement = emptyTag

  def SortableDeletionReason = PlainText.DeletionReason
  def RenderDeletionReason = ProjectWidgets.DeletionReason

}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ColumnRenderers(project: Project, pw: ProjectWidgets) {

  private val cr: Column => ColumnRenderer =
    Memo { c =>
      val cr = c match {
        case Column.Pubid             => pubid
        case Column.ReqType           => reqType
        case Column.Code              => code
        case Column.Title             => title
        case Column.Tags              => tags(Row.tags)
        case Column.Implications(dir) => imps(Row.implications(dir))
        case Column.DeletionReason    => deletionReason
        case Column.CustomField(f, _) =>
          f match {
            case id: CustomField.Text       .Id => cfText(id)
            case id: CustomField.Tag        .Id => tags(Row cfTag id)
            case id: CustomField.Implication.Id => imps(Row cfImp id)
          }
      }
      cr(c)
    }

  def apply(c: Column): ColumnRenderer =
    cr(c)

  private val applicability = ??? //Column.applicability(project.config)

  private def make(render: Row => View): Column => ColumnRenderer =
    c => {
//      val render2 = applicability(c).fn(render)(`N/A`)
//      new ColumnRenderer(c, render2)
      ???
    }

  private def maybeEmpty[A](lens: Optional[Row, Vector[A]], r: Row)(f: Vector[A] => VdomElement): View =
    Render(lens.getOption(r).filter(_.nonEmpty).fold(empty)(f))

//  @deprecated("placeholder is for dev purposes only.", "")
//  private def placeholder =
//    make(Function const render(<.span("∅")))

  private val pubidColumnValue =
    pw.PubidFormat(Plain, *.pubidColumnValue(_), titleFn = _ => None)

  private def pubid = make {
    case r: ReqRow          => Render(pubidColumnValue(r.req))
    case _: ReqCodeGroupRow => `N/A`
  }

  private def reqType = make {
    case r: ReqRow          => Render(pw.reqTypeShort(r.req.reqTypeId))
    case _: ReqCodeGroupRow => `N/A`
  }

  private def code = make {
    case ReqRow(_, _, exp, _, _)        => Render(if (exp.reqCodeTree.nonEmpty) pw.reqCodeTree(exp.reqCodeTree) else pw.reqCodes(exp.reqCodes))
    case ReqCodeGroupRow(_, _, Some(t)) => Render(pw.reqCodeTreeItem(t))
    case ReqCodeGroupRow(_, c, None)    => Render(pw.reqCode(c))
  }

  private def title = make {
    case r: ReqRow          => Render(pw.reqTitle(r.req))
    case r: ReqCodeGroupRow => Render(pw.reqCodeGroupTitle(r.group))
  }

  private def imps(lens: Optional[Row, Vector[Pubid]]) = make {
    case r: ReqRow          => maybeEmpty(lens, r)(pw.implicationList)
    case _: ReqCodeGroupRow => `N/A`
  }

  private def tags(lens: Optional[Row, Vector[ApplicableTagId]]) = make {
    case r: ReqRow          => maybeEmpty(lens, r)(pw.tagList)
    case _: ReqCodeGroupRow => `N/A`
  }

  private def cfText(id: CustomField.Text.Id) = {
    val f = pw.customTextField(id)
    make {
      case r: ReqRow          => Render(f(r.req).fold(empty)(w => w))
      case _: ReqCodeGroupRow => `N/A`
    }
  }

  private def deletionReason = make {
    case r: ReqRow          => RenderDeletionReason.forReq(r.req)(project.config.reqTypes, pw).fold[View](`N/A`)(Render(_))
    case _: ReqCodeGroupRow => RenderDeletionReason.forReqCodeGroup.fold[View](`N/A`)(Render(_))
  }
}
*/
