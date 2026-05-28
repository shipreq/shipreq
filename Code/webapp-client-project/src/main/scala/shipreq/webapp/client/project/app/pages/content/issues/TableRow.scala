package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util.{Allow, ConsolidatedSeq, Disabled, Enabled, ErrorMsg, IfApplicable, Permission}
import shipreq.webapp.base.feature.clipboard.ClipboardKeys
import shipreq.webapp.base.feature.{AsyncFeature, TableNavigationFeature}
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.feature.RenderFeature.FieldKey
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.client.project.widgets.{EditorNavParent, ProjectWidgets}
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.text.Text
import shipreq.webapp.member.project.text.Text.Equality._
import shipreq.webapp.member.ui.BaseStyles

object TableRow {
  type TD = VdomTagOf[html.TableCell]

  final case class Props(row            : Row,
                         columns        : NonEmptyVector[Column],
                         editor         : Option[Reusable[IfApplicable[TagMod => EditorNavParent.Props]]],
                         pubidFormat    : ProjectWidgets.NoCtx#PubidFormat,
                         cmdInvoker     : Action.Cmd ~=> Callback,
                         cmdAsync       : AsyncFeature.Read.D1[Action.Cmd, ErrorMsg],
                         issueCategory  : Option[Reusable[TD]],
                         issueClass     : Option[Reusable[TD]],
                         idBase         : Option[Reusable[TD]],
                         titleBase      : Option[Reusable[TD]],
                         fieldName      : Option[Reusable[TD]],
                         fieldEditorBase: Option[Reusable[TagMod]],
                         fieldActionBase: Option[Reusable[TD]],
                         editability    : Permission,
                        )

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private implicit val tableNavigationFeature = TableNavigationFeature.HasRowSpans

  private val td = <.td(*.tableData, ^.tabIndex := -1)

  private val na = TagMod(*.na, "–")

  private def cellBase(col: Column, addNav: Boolean = true, allowCopy: Boolean = true) = {
    def keys(e: ReactKeyboardEventFromHtml): Callback =
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
          p.fieldName.foreach(cells += _.value)

        case Column.FieldEditor =>
          for (base <- p.fieldEditorBase) {
            val c = p.editor.flatMap(_.value.toOption) match {
              case Some(props) => props(base).renderWithKey(col.key)
              case None        => cellBase(col, allowCopy = false)(base, na)
            }
            cells += c
          }

        case Column.Actions =>
          def content =
            if (row.actions.isEmpty)
              na
            else
              row.actions.mkTagMod(<.br) {

                case a: Action.Button =>
                  import AsyncFeature.Status._
                  val async = p.cmdAsync(a.cmd)
                  val enabled = Enabled.when(p.editability.is(Allow))
                  def ok = a.button(enabled)(^.onClick --> p.cmdInvoker(a.cmd))
                  async match {
                    case None                    => ok
                    case Some(InProgress)        => a.button(Disabled)
                    case Some(Failed(err, _, _)) => TagMod(ok, <.br, BaseStyles.errorPointingUp(err.value))
                  }

                case a: Action.Link =>
                  a.render
              }

          for (base <- p.fieldActionBase) {
            cells += base(content)
          }
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

  private def renderGroupBase(col: Column): ConsolidatedSeq.Group[Any] => Reusable[TD] = g =>
    Reusable.byRef(cellBase(col)(^.rowSpan := g.size))

  private def renderGroupWithCount[A](col: Column, renderName: ConsolidatedSeq.Group[A] => TagMod): ConsolidatedSeq.Group[A] => Reusable[TD] = g => {
    val base = cellBase(col)(renderName(g))
    val result =
      if (g.size == 1)
        base
      else
        base(
          <.div(*.rowspanOuter, "(", <.span(*.rowspanInner, g.size), ")"),
          ^.rowSpan := g.size)
    Reusable.byRef(result)
  }

  private def renderIssueGroup(col: Column): ConsolidatedSeq.Group[String] => Reusable[TD] =
    renderGroupWithCount[String](col, _.value)

  private val consolidateStrings = ConsolidatedSeq.Logic.consolidateByUnivEq[String]

  val consolidateIssueCategories = consolidateStrings(renderIssueGroup(Column.IssueCategory))

  val consolidateIssueClasses = consolidateStrings(renderIssueGroup(Column.IssueClass))

  final case class Id(group: Int, value: Option[ReqCode.Value \/ ReqId])

  object Id {
    implicit def univEq: UnivEq[Id] = UnivEq.derive
    val consolidate = ConsolidatedSeq.Logic.cmp[Id]((a, b) => a.value.isDefined && (a ==* b))(renderGroupBase(Column.Id))
  }

  private val consolidateText = ConsolidatedSeq.Logic.consolidateByUnivEq[(Int, Text.AnyOptional)]

  val consolidateTitles = consolidateText(renderGroupBase(Column.Title))

  final case class Field(group: Int, value: Option[IssueField[EditorFeature.FieldKey]])

  object Field {
    implicit def univEq: UnivEq[Field] = UnivEq.derive

    private val logic = ConsolidatedSeq.Logic.cmp[Field]((a, b) => a.value.isDefined && (a ==* b))

  private def renderFieldNameGroup(col: Column): ConsolidatedSeq.Group[Field] => Reusable[TD] =
    renderGroupWithCount[Field](col, _.value.value.flatMap(_.desc).fold(na)(d => d))

    private def renderEditorBase: ConsolidatedSeq.Group[Any] => Reusable[TagMod] =
      g => Reusable.implicitly(g.size).map(^.rowSpan := _)

// row.fieldOption.flatMap(_.desc).fold(na)(d => d)

    val consolidateNames   = logic(renderFieldNameGroup(Column.FieldName))
    val consolidateEditors = logic(renderEditorBase)
    val consolidateActions = logic(renderGroupBase(Column.Actions))
  }

  // ===================================================================================================================
  // Cells

  def renderEditor[A](column: Column,
                      render: => TagMod,
                      editor: EditorFeature.ReadWrite.ForEditor[A, Any],
                      args  : A): TagMod => EditorNavParent.Props = baseTagMod => {
    val base = cellBase(column, addNav = false)(fieldEditorTagMod, baseTagMod)
    EditorNavParent.Props(base, editor, args, render)
  }
}
