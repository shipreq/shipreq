package shipreq.webapp.client.app.ui.reqtable

import monocle.Optional
import scalacss.ScalaCssReact._
import scalacss.Domain
import japgolly.scalajs.react._, vdom.prefix_<^._
import shipreq.webapp.base.data._
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.util.{Valid, Plain}
import ColumnRenderer._

final class ColumnRenderer(
  val column     : Column,
  val header     : ReactElement,
  val render     : Row => (Status, ReactElement))

object ColumnRenderer {
  sealed trait Status
  case object Normal extends Status
  case object DeadRow extends Status
  case object `N/A` extends Status {
    val element: ReactElement = <.span(*.`N/A`, "–")
    val pair: (Status, ReactElement) = (this, element)
  }
  val statusDomain = Domain.ofValues[Status](Normal, DeadRow, `N/A`)

  val empty: ReactElement = <.span
}

class ColumnRenderers(project: Project, columnName: Column.NameResolver, widgets: ProjectWidgets) {
  @inline private implicit def naobjtoel(n: `N/A`.type) = n.element

  def apply(c: Column): ColumnRenderer = {
    val cr: Column => ColumnRenderer = 
      c match {
        case Column.Pubid          => pubid
        case Column.ReqType        => reqType
        case Column.Code           => code
        case Column.Title          => title
        case Column.Tags           => tags(Row.tags)
        case Column.ImplicationSrc => imps(Row.implicationSrc) //("… ⇒")
        case Column.ImplicationTgt => imps(Row.implicationTgt) //("⇒ …")
        case Column.CustomField(f, _) =>
          f match {
            case id: CustomField.Text       .Id => cfText(id)
            case id: CustomField.Tag        .Id => tags(Row cfTag id)
            case id: CustomField.Implication.Id => imps(Row cfImp id)
          }
      }
    cr(c)
  }

  private val applicability = Applicability(project)

  private def make(render: Row => ReactElement): Column => ColumnRenderer =
    c => makeS(columnName(c), render)(c)

  private def makeS(headerName: String, render: Row => ReactElement): Column => ColumnRenderer = {
    val render2: Row => (Status, ReactElement) =
      row => {
        val e = render(row)
        if (e eq `N/A`.element)
          `N/A`.pair
        else row.live match {
          case Live => (Normal, e)
          case Dead => (DeadRow, e)
        }
      }
    c => {
      val render3 = applicability(c).wrap(render2)(`N/A`.pair)
      new ColumnRenderer(c, <.span(headerName), render3)
    }
  }

  private def maybeEmpty[A](lens: Optional[Row, Vector[A]], r: Row)(f: Vector[A] => ReactElement): ReactElement =
    lens.getOption(r).filter(_.nonEmpty).fold(empty)(f)

  @deprecated("placeholder is for dev purposes only.", "")
  private def placeholder =
    makeS("∅", Function const <.span("∅"))

  private def pubid = make {
    case GenericReqRow(req, _, _) => widgets.pubidColumnValue(req.pubid)
    case _: ReqCodeGroupRow       => `N/A`
  }

  private def reqType = make {
    case GenericReqRow(req, _, _) => widgets.reqType(req.reqTypeId)
    case _: ReqCodeGroupRow       => `N/A`
  }

  private def code = make {
    case GenericReqRow(_, exp, _)       => widgets.reqCodes(exp.reqCodeTree, exp.reqCodes)
    case ReqCodeGroupRow(_, _, Some(t)) => widgets.reqCodeTreeItem(t)
    case ReqCodeGroupRow(_, c, None)    => widgets.flatReqCode(c)
  }

  private def title = make {
    case GenericReqRow(req, _, _) => widgets.reqTitle(req)
    case ReqCodeGroupRow(g, _, _) => widgets.reqCodeGroupTitle(g)
  }

  private def imps(lens: Optional[Row, Vector[Pubid]]) = make {
    case r: GenericReqRow   => maybeEmpty(lens, r)(widgets.pubidRefList(Plain, Valid))
    case _: ReqCodeGroupRow => `N/A`
  }

  private def tags(lens: Optional[Row, Vector[ApplicableTagId]]) = make {
    case r: GenericReqRow   => maybeEmpty(lens, r)(widgets.tagList)
    case _: ReqCodeGroupRow => `N/A`
  }

  private def cfText(id: CustomField.Text.Id) = {
    val f = widgets.customTextField(id)
    make {
      case GenericReqRow(req, _, _) => f(req.id).fold(empty)(w => w)
      case _: ReqCodeGroupRow       => `N/A`
    }
  }
}
