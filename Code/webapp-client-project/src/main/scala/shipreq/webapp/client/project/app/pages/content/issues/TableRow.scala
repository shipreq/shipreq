package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import scalaz.\/
import shipreq.base.util.{ConsolidatedSeq, ErrorMsg, IfApplicable}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.clipboard.ClipboardKeys
import shipreq.webapp.base.feature.{AsyncFeature, TableNavigationFeature}
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.feature.RenderFeature.FieldKey
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.lib.EditorNavParent
import shipreq.webapp.client.project.widgets.ProjectWidgets

object TableRow {
  type TD = VdomTagOf[html.TableCell]

  final case class Props(row             : Row,
                         columns         : NonEmptyVector[Column],
                         editor          : Option[Reusable[IfApplicable[EditorNavParent.Props]]],
                         pubidFormat     : ProjectWidgets.NoCtx#PubidFormat,
                         cmdInvoker      : Action.Cmd ~=> Callback,
                         cmdAsync        : AsyncFeature.Read.D1[Action.Cmd, ErrorMsg],
                         issueCategory   : Option[Reusable[TD]],
                         issueClass      : Option[Reusable[TD]],
                         idBase          : Option[Reusable[TD]],
                         titleBase       : Option[Reusable[TD]],
                        )

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private implicit val tableNavigationFeature = TableNavigationFeature.HasRowSpans

  private val td = <.td(*.tableData, ^.tabIndex := -1)

  private val na = TagMod(*.na, "–")

  private def cellBase(col: Column, addNav: Boolean = true, allowCopy: Boolean = true) = {
    def keys(e: ReactKeyboardEventFromHtml): CallbackOption[Unit] =
      tableNavigationFeature.Keys.handlerFn(e).when(addNav) | ClipboardKeys.copy.generic(e).when(allowCopy)

    td(
      ^.key := col.key,
      ^.onKeyDown ==> keys)
  }

  final val fieldEditorAttr = "data-fe"

  private val fieldEditorTagMod: TagMod =
    VdomAttr(fieldEditorAttr) := "1"

  private def render(p: Props): VdomElement = {
    import p.{row, pubidFormat}

    val cells = VdomArray.empty()

    for (col <- p.columns) {

      def addTD(content: TagMod, addNav: Boolean = true, allowCopy: Boolean = true) =
        cells += cellBase(col, addNav = addNav, allowCopy = allowCopy)(content)

      col match {

        case Column.IssueCategory =>
          p.issueCategory.foreach(cells += _.value)

        case Column.IssueClass =>
          p.issueClass.foreach(cells += _.value)

        case Column.Id =>
          for (base <- p.idBase) {
            val c = row match {
              case r: Row.ForReq         => pubidFormat(r.req)
              case r: Row.ForRcg         => r.renderer(FieldKey.Code).getOrElse(na)
              case _: Row.ForConfig      => na
              case _: Row.ForManualIssue => na
            }
            cells += base(c)
          }

        case Column.Title =>
          for (base <- p.titleBase) {
            val c = row match {
              case r: Row.ForGenericReq  => r.renderer(FieldKey.Title).getOrElse(na)
              case r: Row.ForUseCase     => r.renderer(FieldKey.Title).getOrElse(na)
              case r: Row.ForUseCaseStep => r.ucRenderer(FieldKey.Title).getOrElse(na)
              case r: Row.ForRcg         => r.renderer(FieldKey.CodeGroupTitle).getOrElse(na)
              case _: Row.ForManualIssue => na // This appears in Field Editor
              case _: Row.ForConfig      => na
            }
            cells += base(c)
          }

        case Column.FieldName =>
          addTD(row.fieldOption.flatMap(_.desc).fold(na)(d => d))

        case Column.FieldEditor =>
          p.editor.flatMap(_.value.toOption) match {
            case Some(props) => cells += props.renderWithKey(col.key)
            case None        => addTD(na, allowCopy = false)
          }

        case Column.Actions =>
          val content =
            if (row.actions.isEmpty)
              na
            else
              row.actions.mkTagMod(<.br) {

                case a: Action.Button =>
                  import AsyncFeature.Status._
                  val async = p.cmdAsync(a.cmd)
                  def ok = a.button(Enabled)(^.onClick --> p.cmdInvoker(a.cmd))
                  async match {
                    case None                    => ok
                    case Some(InProgress)        => a.button(Disabled)
                    case Some(Failed(err, _, _)) => TagMod(ok, <.br, BaseStyles.errorPointingUp(err.value))
                  }

                case a: Action.Link =>
                  a.render
              }

          addTD(content, allowCopy = false)
      }
    }

    <.tr(cells)
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(shouldComponentUpdate)
    .build

  // ===================================================================================================================
  // Consolidation

  private type RenderGroup[-A] = ConsolidatedSeq.Group[A] => Reusable[TD]

  private def renderGroupBase(col: Column): RenderGroup[Any] =
    g => Reusable.byRef(cellBase(col)(^.rowSpan := g.size))

  private def renderIssueGroup(col: Column): RenderGroup[String] = g => {
    val base = cellBase(col)(g.value)
    val result =
      if (g.size == 1)
        base
      else
        base(
          <.div(*.rowspanOuter, "(", <.span(*.rowspanInner, g.size), ")"),
          ^.rowSpan := g.size)
    Reusable.byRef(result)
  }

  private val consolidateStrings = ConsolidatedSeq.Logic.consolidateByUnivEq[String]

  val consolidateIssueCategories = consolidateStrings(renderIssueGroup(Column.IssueCategory))

  val consolidateIssueClasses = consolidateStrings(renderIssueGroup(Column.IssueClass))

  final case class Id(group: Int, value: Option[ReqCode.Value \/ ReqId])

  object Id {
    implicit def univEq: UnivEq[Id] = UnivEq.derive
    val consolidate = ConsolidatedSeq.Logic.cmp[Id]((a, b) => a.value.isDefined && (a ==* b))(renderGroupBase(Column.Id))
  }

  private val consolidateText = ConsolidatedSeq.Logic.consolidateByUnivEq[(Int, Text.AnyOptional)]

  val consolidateTitle = consolidateText(renderGroupBase(Column.Title))

  // ===================================================================================================================
  // Cells

  def renderEditor[A](column: Column,
                      render: => TagMod,
                      editor: EditorFeature.ReadWrite.ForEditor[A, Any],
                      args  : A): EditorNavParent.Props = {
    val base = cellBase(column, addNav = false)(fieldEditorTagMod)
    EditorNavParent.Props(base, editor, args, render)
  }
}
