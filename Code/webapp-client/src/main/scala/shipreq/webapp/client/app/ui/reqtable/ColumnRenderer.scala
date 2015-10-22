package shipreq.webapp.client.app.ui.reqtable

import monocle.Optional
import scalacss.ScalaCssReact._
import scalacss.Domain
import japgolly.scalajs.react._, vdom.prefix_<^._
import shipreq.base.util.Valid
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.ProjectText
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.lib.Plain
import ColumnRenderer._

final class ColumnRenderer(
  val column: Column,
  val render: Row => (Status, ReactElement))

object ColumnRenderer {
  sealed trait Status

  case object Normal  extends Status
  case object DeadRow extends Status
  case object `N/A`   extends Status {
    val tag    : ReactTag               = <.span(*.`N/A`, "–")
    val element: ReactElement           = tag
    val pair   : (Status, ReactElement) = (this, element)
  }

  val statusDomain = Domain.ofValues[Status](Normal, DeadRow, `N/A`)

  val emptyTag: ReactTag     = <.span
  val empty   : ReactElement = emptyTag

  // -------------------------------------------------------------------------------------------------------------------

  object RenderDeletionReason extends ProjectText.DeletionReasonFormatter[ReactTag] {
    override type PT = ProjectWidgets

    override protected def `n/a` =
      `N/A`.tag

    override protected def noReasonGiven =
      emptyTag

    override protected def reqTypeIsDead(pt: PT, rt: ReqType) =
      <.span(
        UiText.ColumnNames.reqType + " ",
        pt.reqType(rt.reqTypeId),
        " is deleted.")
  }

  object SortableDeletionReason extends ProjectText.DeletionReasonFormatter[String] {
    override type PT = ProjectText[String]
    override protected def `n/a` = ""
    override protected def noReasonGiven = ""
    override protected def reqTypeIsDead(pt: PT, rt: ReqType) =
      UiText.ColumnNames.reqType + " " + rt.mnemonic.value + " is deleted."
  }
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
        case Column.DeletionReason => deletionReason
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

  private def make(render: Row => ReactElement): Column => ColumnRenderer = {
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
      new ColumnRenderer(c, render3)
    }
  }

  private def maybeEmpty[A](lens: Optional[Row, Vector[A]], r: Row)(f: Vector[A] => ReactElement): ReactElement =
    lens.getOption(r).filter(_.nonEmpty).fold(empty)(f)

  @deprecated("placeholder is for dev purposes only.", "")
  private def placeholder =
    make(Function const <.span("∅"))

  private def pubid = make {
    case r: GenericReqRow   => widgets.pubidColumnValue(r.req.pubid)
    case _: ReqCodeGroupRow => `N/A`
  }

  private def reqType = make {
    case r: GenericReqRow   => widgets.reqType(r.req.reqTypeId)
    case _: ReqCodeGroupRow => `N/A`
  }

  private def code = make {
    case GenericReqRow(_, _, exp, _, _) => widgets.reqCodes(exp.reqCodeTree, exp.reqCodes)
    case ReqCodeGroupRow(_, _, Some(t)) => widgets.reqCodeTreeItem(t)
    case ReqCodeGroupRow(_, c, None)    => widgets.flatReqCode(c)
  }

  private def title = make {
    case r: GenericReqRow         => widgets.reqTitle(r.req)
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
      case r: GenericReqRow   => f(r.req).fold(empty)(w => w)
      case _: ReqCodeGroupRow => `N/A`
    }
  }

  private def deletionReason = make {
    case r: GenericReqRow   => RenderDeletionReason.req(project, widgets, r.req)
    case _: ReqCodeGroupRow => RenderDeletionReason.reqCodeGroup
  }
}
