package shipreq.webapp.client.project.app

import japgolly.scalajs.react.vdom.html_<^.{^ => ^^, _}
import japgolly.univeq._
import shipreq.webapp.base.CssSettings._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.data.{Dead, Live, StaticField}
import shipreq.webapp.base.data._
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.base.ui.semantic.{Colour, Label, UsesSemanticUiManually}
import shipreq.webapp.client.project.widgets._

object Style extends StyleSheet.Inline {
  import dsl._

  /** Domains */
  object D {
    val live     = Domain.ofValues[Live]    (Live, Dead)
    val validity = Domain.ofValues[Validity](Valid, Invalid)
    val enabled  = Domain.ofValues[Enabled] (Enabled, Disabled)
    val on       = Domain.ofValues[On]      (On, Off)

    val dragStatus = {
      import DragToReorder._
      Domain.ofValues[Status](Normal, DragSource, Tombstone)
    }

    val `live * live`     = live *** live
    val `live * on`       = live *** on
    val `live * validity` = live *** validity

    val ucStepIndent = Domain.ofRange(0 until StaticField.useCaseStepTrees.iterator.map(_.maxDepth).max)
  }

  private def monospace =
    fontFamily :=! "monospace"

  /** Drag'n'drop handle Ξ */
  private val dragHnd = style(
    color(c"#000"))

  /** An empty style */
  private val empty = style()

  private val hasErrorBackground =
    backgroundColor(c"#fee")

  private val hasErrorColor =
    color(c"#c00")

  private val errorRedOnRed = mixin(
    hasErrorColor,
    hasErrorBackground)

  private def deadColumnLabel(live: Live) =
    mixinIf(live is Dead)(textDecoration := ^.lineThrough)

  private val hasTitle = Pseudo.Custom("[title]", PseudoType.Element)

  private val hoverShowsInfo = hasTitle(cursor.help)

  private def hasError = errorRedOnRed

  private val deadMixin = mixin(
    textDecoration := ^.lineThrough)

  private def deadMaybeValid(v: Validity) = v match {
    case Valid   => deadAndNotError
    case Invalid => deadAndError
  }

  private val deadAndNotError = mixin(
    deadMixin,
    color(c"#999"))

  private val deadAndError = mixin(
    deadMixin,
    hasError)

  private val deadFilledCell = style(
    opacity(0.7),
    // This ↓ needs to be kept in sync with detailTableKey which uses .04 but .06 is required when opacity=0.7
    backgroundColor(rgba(0, 0, 0, .06)))

  val svgGraph = style(
    unsafeChild("svg")(
      maxWidth(100 %%)))

  private val selectionCellBase = style(
    width(24.px).important,
    textAlign.center.important)

  val layout = style(
    unsafeRoot(".ui.button")(marginRight(`0`).important))

  val noFilterResultsCont = style(
    padding(2 em).important)

  object generic {
    val table = style(
      margin(`0`).important)

    val tableHeaderBase = style(
      padding(4.px).important,
      &.focus(outlineColor(BaseStyles.focus.colour(1))))

    val tableDataBase = style(
      padding(4.px).important,
      verticalAlign.top.important,
      (borderLeft :=! "1px solid #2224261a").important) // Without this, rowspan break semantic UI table borders
  }

  // ===================================================================================================================
  object home {

    val projectHeader = style(
      paddingBottom(1 rem),
      paddingTop(0.5 rem))

    val cardHeader = style(
      &.firstChild(
        marginTop(`0`).important),
      &.not(_.firstChild)(
        marginTop(1.8 em).important),
      marginBottom(0.2 em).important,
      paddingTop(0.8 em).important,
      paddingBottom(0.3 em).important)

    val cardsCont = style(
      marginTop(`0`).important)

    val linkCard = style(
      cursor.pointer)

    val cardIconCont = style(
      textAlign.center,
      paddingTop(2.7 em).important,
      height(6.8 em).important,
      flexGrow(0).important)

    val cardIcon = style(
      fontSize(3 rem).important)

    val reqLookupPromptHasError = style(
      borderColor(red).important)
  }

  // ===================================================================================================================
  object impgraphPage {

    val filterDeadButton = style(
      textAlign.right)

    val graph = style(
      textAlign.center,
      margin.horizontal(auto))
  }

  // ===================================================================================================================
  // Config screens
  object cfg {

    val deadMnemonic = style(
      marginTop(0.4 ex),
      color(c"#aaa"),
      textDecoration := ^.lineThrough)

    // HACK!
    val fields = style(
      unsafeChild(">table>*>*>td:nth-child(1)")(padding(1 ex).important, textAlign.center),
      unsafeChild(">table>*>*>td:nth-child(1) .draghandle:hover")(cursor.grab),
      unsafeChild(">table>*>*>td:nth-child(4) input")(monospace, width((Grammar.fieldRefKey.length.total.last + 1).ch)),
      unsafeChild(">table>*>*>td:nth-child(6) button")(marginLeft(1 ex)))

    // HACK!
    val issues = style(
      unsafeChild(">div>table>*>*>td:nth-child(1) input")(monospace, width((Grammar.hashRefKey.length.total.last + 1).ch)),
      unsafeChild(">div>table>*>*>td:nth-child(2)")(width(100 %%)),
      unsafeChild(">div>table>*>*>td:nth-child(2) textarea")(width(100 %%)),
      unsafeChild(">.other input")(marginRight(0.6 ex).important, marginBottom(0.9 ex).important))

    // HACK!
    val reqTypes = style(
      unsafeChild(">table>*>*>td:nth-child(1)")(monospace),
      unsafeChild(">table>*>*>td:nth-child(1) input")(monospace, width((Grammar.reqTypeMnemonic.length.total.last + 1).ch)),
      unsafeChild(">table>*>*>td:nth-child(2)")(width(100 %%)),
      unsafeChild(">table>*>*>td:nth-child(2) input")(width(100 %%)))

    // HACK!
    val tags = style(
      unsafeChild(">table>*>*>td:nth-child(1)")(width(50 %%)),
      unsafeChild(">table>*>*>td:nth-child(1) input")(width(100 %%)),
      unsafeChild(">table>*>*>td:nth-child(2)")(monospace),
      unsafeChild(">table>*>*>td:nth-child(2) input")(monospace, width((Grammar.hashRefKey.length.total.last + 1).ch)),
      unsafeChild(">table>*>*>td:nth-child(4)")(width(50 %%)),
      unsafeChild(">table>*>*>td:nth-child(4) textarea")(width(100 %%)),
      unsafeChild(">table>*>*>td:nth-child(5)")(whiteSpace.nowrap),
      unsafeChild(">table>*>*>td:nth-child(5) button+button")(marginLeft(1.ex)),
      unsafeChild(">table .focusrow>td")(backgroundColor(c"#f0f8ff")),
      unsafeChild(">section table td+td")(paddingLeft(3 em)),
    )
  }

  // ===================================================================================================================
  object reqtable {

    // nearly everything here is !important because of stupid Semantic UI

    private def pageVGap = 1.25 em

    object page {

      val ctrlHGap = 1.2 ex

      val viewRow = style(display.flex)

      val viewRowSV = style(
        flexGrow(1),
        paddingRight(1.rem))

      val viewCtrls = style(
        display.flex,
        alignContent.center,
        margin(v = pageVGap, h = `0`),
        unsafeChild("> *:not(:first-child)")(marginLeft(ctrlHGap)))

      def actionCtrls = viewCtrls

      val actionCtrlButtonWrap = style(
        marginRight(ctrlHGap),
        display.inline)

      val summary = style(
        flexGrow(1),
        textAlign.right)

      val flexGap = style(flexGrow(1))

      val filterDeadButtonContainer = style(
        paddingRight(`0`).important)
    }

    object creation {

      val buttonOuter = style(
        marginRight(`0`).important)

      val buttonDropdown = style(
        color(c"#eee").important,
        backgroundColor(c"#00a632").important)

      val formOuter = style(
        margin(v = pageVGap, h = `0`))

      val formTable = style(
        margin(`0`),
        (boxShadow := "0 2px 4px 0 rgba(20,60,20,.16),0 2px 10px 0 rgba(20,60,20,.12)").important,
        unsafeChild(">tbody>tr>td")(
          borderTop.none.important,
          borderLeft.none.important,
          borderRight.none.important))

      val formHeaderCell = style(
        borderLeft.none.important,
        borderRight.none.important,
        paddingTop(0.4 em).important,
        paddingBottom(0.4 em).important)

      val formMiddleRow = style(
        verticalAlign.top)

      val formBottomRow = style(
        textAlign.right.important,
        unsafeChild(">*")(
          &.not(_.lastChild)(marginRight(2 ex).important),
          &.lastChild(marginRight(`0`).important)))

      val formCreateButton = style(
        (background := "#fff").important,
        color(c"#080").important,
        borderColor(c"#080").important,
        fontWeight.bold.important)
    }

    object table {
      def table = generic.table
      @inline private def headerBase = generic.tableHeaderBase

      val columnHeader = styleF(D.live *** D.dragStatus) { case (live, status) => styleS(
        headerBase,
        deadColumnLabel(live),
        cursor.pointer.important, // Because click affects sorting
        (status match {
          case DragToReorder.Normal => mixin()
          case DragToReorder.DragSource | DragToReorder.Tombstone => mixin(
            opacity(.4).important,
            border(2 px, dashed, c"#779").important)
        }): StyleS
      )}

      private val cellBase = styleF(D.`live * on`)(i => styleS(
        generic.tableDataBase,
        i match {
          case (Live, Off) => mixin()
          case (Dead, Off) => mixin(backgroundColor(c"#f5f5f5"))
          case (Live, On ) => mixin(backgroundColor(hsla(228, 90 %%, 75 %%, .12)))
          case (Dead, On ) => mixin(backgroundColor(hsla(228, 74 %%, 33 %%, .11)))
        }))

      val dataCell = styleF(D.`live * on`)(i => styleS(
        cellBase(i),
        &.focus(BaseStyles.focus.glowOutline)))

      val selectionColumnHeader = style(selectionCellBase, headerBase)
      val selectionDataCell     = styleF(D.`live * on`)(i => styleS(selectionCellBase, dataCell(i)))
      val selectionCheckbox     = style(&.focus(outlineColor(BaseStyles.focus.colour(1))))

      val pubidColumnValue = styleF(D.live)(a => styleS(
        display.inline,
        whiteSpace.nowrap,
        mixinIf(a is Dead)(deadAndNotError)))

      val `N/A` = style(
        color(c"#666"),
        margin.horizontal(auto))
    }

    object filterEditor {
      private def colourValid = color(c"#2C662D").important

      val input = styleF(D.validity) {
        case Valid => styleS(
          backgroundColor(c"#FCFFF5").important,
          colourValid,
          (boxShadow := "0px 0px 0px 1px #A3C293 inset, 0px 0px 0px 0px rgba(0, 0, 0, 0)").important)
        case Invalid => styleS()
      }

      val filterIcon = styleF(D.validity) {
        case Valid   => styleS(opacity(1).important, colourValid)
        case Invalid => styleS(opacity(1).important, color(c"#9f3a38").important)
      }
    }

    object sortEditor {

      val header = style(
        display.inlineBlock,
        verticalAlign.top,
        marginTop(1.3.ex))

      val dragArea = style(
        display.inlineBlock,
        paddingRight(10.ex)) // ← Gives a bit more room to drag to tail, rather than outside

      val draggableCriterion = styleF(D.dragStatus)(status =>
        styleS(
          marginLeft(2 ex),
          borderRadius(4.px),
          cursor.pointer, // Because click changes sort direction
          // inlineFlex required below to keep the entire row at the right height
          // inlineBlock adds extra height and causes height differences between filter section & column button
          (status match {
            case DragToReorder.Normal     => mixin(display.inlineFlex)
            case DragToReorder.DragSource => mixin(display.inlineFlex, opacity(.4), border(2 px, dashed, c"#000"))
            case DragToReorder.Tombstone  => mixin(display.none)
          }): StyleS
        ))

      val criterionBorder = style(
        borderRadius(4.px),
        boxShadow := "0 0 0 1px rgba(34,36,38,.15) inset")

      val name = styleF(Domain.boolean)(conclusive =>
        styleS(
          border.none,
          padding(v = 0.75.em, h = 1.5.em),
          verticalAlign.middle,
          color(rgba(0,0,0,.6)),
          fontSize(0.85714286.rem),
          mixinIf(conclusive)(fontWeight.bold)))

      val sortMethod = style(
        border.none,
        backgroundColor(rgba(0,0,0,.05)),
        verticalAlign.middle,
        textAlign.center,
        padding(`0`),
        width(2.57142857.em),
        borderTopRightRadius.inherit,
        borderBottomRightRadius.inherit)

      private val sortMethodBase = mixin(
        display.block,
        width(100.%%),
        opacity(0.5))

      private val sortMethodHalf = mixin(
        sortMethodBase,
        height(0.56.em))

      val sortMethodFull       = style(sortMethodBase, height(0.6.em))
      val sortMethodHalfTop    = style(sortMethodHalf, marginBottom(0.26.em))
      val sortMethodHalfBottom = style(sortMethodHalf)
    }

    object savedViews {

      val menu = TagMod(^^.display.flex, ^^.flexWrap.wrap)

      val activeItem = style(
        fontWeight._700.important,
        (boxShadow := "none").important,
        borderColor(c"#F2711C").important,
        color(c"#F2711C").important)

    }
  }

  // ===================================================================================================================
  object deletionRestorationForms {

    val main = style(
      margin.horizontal(auto),
      maxWidth(144 ex),
      paddingTop(1 em),
      padding.horizontal(2 em))

    val reqHelp = style(marginTop(2 em))

    def deadTextColour = hsl(0, 71 %%, 50 %%)
    def deadTextColour(a: Double) = hsla(0, 71 %%, 50 %%, a)

    val reqTable = style(
      marginTop(0.6 em).important,
      unsafeChild(">thead>tr>th:last-child")(textAlign.center),
      unsafeChild(">thead>tr:nth-child(2)>th")(textAlign.center, paddingTop(`0`).important),
      unsafeChild(">thead>tr>th")(padding(0.44 em).important),
      unsafeChild(">tbody>tr>td")(padding(0.44 em).important, lineHeight(1.2 em).important, verticalAlign.top))

    val reqTableImpsCell              = style(width(8 ex))
    val reqTableHeaderImpsTop         = style(borderBottom.none.important)
    val reqTableHeaderImpsBottomLeft  = style(reqTableImpsCell, borderLeft(1 px, solid, rgba(34, 36, 38, .1)).important)
    val reqTableHeaderImpsBottomRight = style(reqTableImpsCell, borderLeft.none.important)
    val reqTableHeaderImpsIcon        = style(margin(`0`).important)

    /** @param i > 1 */
    def indentWidth(i: Int): String =
      s"${(i - 1) * 2}ex"

    def reqTableSelCol = selectionCellBase


    val reqTablePubidCell = style(display.flex, whiteSpace.nowrap)
    val reqTableTreeIndicator = style(color(c"#a8a8a8"))

    val pubid = styleF(D.live)(l => styleS(
      flexGrow(1),
      mixinIf(l is Dead)(color(deadTextColour)),
      &.not(_.firstChild)(paddingLeft(0.5 ex))))

    val reqTableTitle = styleF(D.live)(l => styleS(mixinIf(l is Dead)(color(deadTextColour))))
    val reqTableImps = styleF(D.live)(l => styleS(reqTableTitle(l), whiteSpace.nowrap))
  }

  // ===================================================================================================================
  object deletionForm {
    import deletionRestorationForms.deadTextColour

    def main    = deletionRestorationForms.main
    def reqHelp = deletionRestorationForms.reqHelp

    val reqTableRow = styleF(D.live) {
      case Live => styleS(
        &.hover(backgroundColor(rgba(0, 192, 0, .08))))
      case Dead => styleS(
        backgroundColor(deadTextColour(.06)),
        &.hover(backgroundColor(deadTextColour(.15))))
    }

    val deletionReasonHeader = style(marginBottom(0.4 em))

    val cancelButton = style(marginBottom(1.2 em).important)

    val bottomSections = style(display.flex, marginTop(4 em))
    val bottomSectionL = style(flexGrow(1), alignSelf.flexEnd)
    val bottomSectionR = style(paddingLeft(6 em), alignSelf.flexEnd)
  }

  // ===================================================================================================================
  object restorationForm {
    import deletionRestorationForms.deadTextColour

    def main    = deletionRestorationForms.main
    def reqHelp = deletionRestorationForms.reqHelp

    val reqTableRow = styleF(D.live) {
      case Live => styleS(
        backgroundColor(rgba(0, 192, 0, .07)),
        &.hover(backgroundColor(rgba(0, 192, 0, .16))))
      case Dead => styleS(
        &.hover(backgroundColor(deadTextColour(.1))))
    }

    val bottomSection = style(marginTop(4 em), textAlign.right)
    val buttonGap     = style(width(2.6 em), display.inlineBlock)
  }

  // ===================================================================================================================
  object reqdetail {

    val errorCont = style(maxWidth(100 ex), margin(11 em, auto).important)
    val errorDesc = style(marginTop(1 em).important)
    val errorBr   = style(lineHeight(1.5 em))

    val headerRow = style(
      display.flex)

    val headerPubid = style(
      whiteSpace.nowrap,
      paddingRight(0.4 rem))

    val headerTitle = style(
      flexGrow(1),
      paddingLeft(0.4 rem))

    val headerText = styleF(D.live)(live => styleS(
      margin(`0`).important,
      mixinIf(live is Dead)(
        textDecoration := "line-through",
        opacity(0.4))))

    val headerFilterDeadButton = style(
      paddingLeft(BaseStyles.pageMargin))

    private def innerCellBorderColour =
      rgba(34, 36, 38, 0.1)

    val detailTable = style(
      border(1 px, solid, rgba(34, 36, 38, 0.15)),
      borderCollapse.separate,
      borderRadius(0.28571429 rem),
      borderSpacing(`0`),
      boxShadow := "none",
      marginTop(1.5 rem),
      width(100 %%),

      unsafeChild(">tbody >tr:not(:first-child) >*")(
        borderTop(1 px, solid, innerCellBorderColour)),

      unsafeChild(">tbody >tr >td")(
        borderLeft(1 px, solid, innerCellBorderColour)))

    private def detailTableCell = mixin(
      padding(0.6 rem, 0.7 rem),
      verticalAlign.top,
      &.focus(BaseStyles.focus.glowOutline))

    val detailTableKey = styleF(D.live)(live => styleS(
      detailTableCell,
      textAlign.left,
      wordWrap.breakWord,
      // whiteSpace.nowrap,
      mixinIf(live is Live)(backgroundColor(rgba(0, 0, 0, .04))),
      mixinIf(live is Dead)(deadFilledCell, textDecoration := "line-through")))

    val detailTableValue = styleF(D.live)(live => styleS(
      detailTableCell,
      width(100 %%),
      mixinIf(live is Dead)(deadFilledCell)))

    val generalImpsCont = style(
      width(100 %%))

    val generalImpsSide = style(
      &.focus(BaseStyles.focus.glowOutline),
      width(50 %%),
      textAlign.center)

    val generalImpsMiddle = style(
      fontWeight.bold,
      padding(`0`, 1 ex),
      whiteSpace.nowrap)

    object useCaseStep {

      val container = style(
        display.flex,
        justifyContent.flexEnd, // So that controls in tail-step rows appear on the right.
        width(100 %%),
        &.not(_.firstChild)(marginTop(0.2 rem)))

      val header = styleF(D.ucStepIndent)(lvl =>
        styleS(
          boxSizing.contentBox,
          color(c"#444"),
          lvl match {
            case 0 => styleS(fontWeight.bold,    width(6 ex)) // 123.0
            case 1 => styleS(paddingLeft( 4 ex), width(4 ex)) // 99.
            case 2 => styleS(paddingLeft( 8 ex), width(4 ex)) // cv.
            case 3 => styleS(paddingLeft(12 ex), width(4 ex)) // xviii.
            case 4 => styleS(paddingLeft(16 ex), width(4 ex)) // 99.
          }
        )
      )

      val deadStepLabel = style(
        color(c"#bbb"),
        textDecoration := ^.lineThrough)

      val body = style(
        flexGrow(1),
        paddingLeft(0.6 ex),
        &.focus(BaseStyles.focus.glowOutline))

      val ctrls = style(
        width(8.9 rem),
        textAlign.right)

      private val ctrlButton = style(
        unsafeChild("i")(fontSize(1.2 rem).important),
        margin(`0`).important,
        padding(0.3 rem).important,
        &.disabled(opacity(0.3).important))

      val ctrlButtonInsert     = style(ctrlButton, &.hover(color(c"#21BA45").important))
      val ctrlButtonDelete     = style(ctrlButton, &.hover(color(c"#DB2828").important))
      def ctrlButtonRestore    = ctrlButtonInsert

      def ctrlButtonShift(d: LeftRight) = ctrlButton
    }
  }

  // ===================================================================================================================
  object issues {

    private def gapSize = 1 rem

    val pageRow1 = style(display.flex)
    val pageNew  = style(flexGrow(1), marginRight(gapSize))

    val pageRow2 = style(margin.vertical(gapSize), display.flex)
    def pageSort = pageNew

    val emptyCont = style(marginTop(gapSize))

    val newIssueCont = style(display.flex)
    val newIssueForm = style(flexGrow(1), marginLeft(1.5 ex))

    def table       = generic.table
    def tableHeader = generic.tableHeaderBase

    val tableData   = style(
      generic.tableDataBase,
      &.focus(BaseStyles.focus.glowOutline))

    val rowspanOuter = style(
      display.inline,
      color(c"#999"),
      paddingLeft(1.ex),
      whiteSpace.nowrap)

    val rowspanInner = style(
      color(c"#c66"))

    val pubidColumnValue = style(
//      display.inline,
      whiteSpace.nowrap)

    val na = style(
      color(c"#666"))

    val actionButton = style(
      textAlign.left.important,
      whiteSpace.nowrap.important,
      color(c"#fff").important,
      (background := "#2f9d3e").important,
      padding(0.4 em, 0.8 em).important)
  }

  // ===================================================================================================================
  object widgets {

    private val refColour = color(c"#2363A1")

    val blankLine = style(display.block, height(1 em))

    val ul = style(paddingLeft(2.4 ex))

    val blankButMandatory = style(
      errorRedOnRed,
      textAlign.center,
      height(100 %%),
      width(11 ex),
      borderRadius(0.3 ex))

    private def tagBase(live: Live) = mixin(
      mixinIf(live is Dead)(&.not(_.hover)(textDecoration := ^.lineThrough)),
      hoverShowsInfo)

    private val tagLabelColour: ((Live, Validity)) => String = {
      case (Live, Valid  ) => "blue"
      case (Live, Invalid) => ""
      case (Dead, _      ) => "grey"
    }

    @UsesSemanticUiManually
    val tag = styleF(D.`live * validity`)(lv => styleS(
      tagBase(lv._1),
      padding(4 px, 6 px).important,
      mixinIf(lv._2 is Invalid)(hasErrorBackground.important, hasErrorColor.important),
      addClassName(s"ui label ${tagLabelColour(lv)}")))

    val tagInText = styleF(D.`live * validity`){ case (l, v) => styleS(
      tagBase(l),
      mixinIf(l is Live)(refColour),
      mixinIf(l is Dead)(deadMaybeValid(v)))
    }

    val reqTypeShort = styleF(D.live)(a => styleS(
      hoverShowsInfo,
      mixinIf(a is Dead)(deadAndNotError)))

    val issue = styleF(D.`live * live`) { case (liveCtx, liveIssue) => styleS(
      mixinIf(liveCtx is Live)(hasError),
      mixinIf(liveIssue is Dead)(textDecoration := ^.lineThrough))
    }

    val erroneousUseCaseStepRef = style(hasError)

    val issueDesc = style(
      padding.horizontal(0.7 ex))

    val pastPubid = style(deadMaybeValid(Valid))

    val reqRef = styleF(D.`live * validity`){ case (l, v) => styleS(
      unsafeExt("span" + _)(hoverShowsInfo), // usually tag is <a> which links to ReqDetail
      mixinIf(l is Live)(refColour),
      mixinIf(l is Dead)(deadMaybeValid(v))
    )}

    def codeGroupRef = reqRef

    def useCaseStepRef = reqRef

    val math = style(margin.horizontal(0.8 ex))
    val mathFail = style(math, hasError)

    // Fucking bootstrap
    private val reqCodePre = mixin(
      margin.`0`,
      padding.`0`,
      background := ^.unset,
      border.none,
      fontSize(12 px),
      lineHeight(1.2 em),
      wordBreak.keepAll,
      wordWrap.normal,
      whiteSpace.pre
    )
    private val reqCodeTreePre = mixin(reqCodePre, display.inline)

    val reqCodeTreeIndent = style(reqCodeTreePre, color(c"#dadada"))
    val reqCodeTreeCode = style(reqCodeTreePre)
    val reqCodeFlat = style(reqCodePre, display.block, overflowY.hidden)

    val useCaseStepTextAndFlow_cont = styleF(D.live)(l => styleS(
      display.flex,
      mixinIf(l is Dead)(deadFilledCell)))

    val useCaseStepTextAndFlow_text = style()
    val useCaseStepTextAndFlow_flow = style()

    val useCaseStepFlowClause = style(marginLeft(0.5 ex), display.inline)

    val useCaseStepFlowElement = styleF(D.`live * validity`)(i => styleS(
      hoverShowsInfo,
      marginLeft(0.5 ex),
      reqRef(i)))

    object reqTypeSelector {
      val dropdown = style(
        backgroundColor(BaseStyles.editor.backgroundColor).important,
        borderColor(BaseStyles.editor.borderColor).important,
        marginRight(1 ex).important)

      val buttons = style()

      val commit = style(
        &.hover(color(c"#21BA45").important))

      val abort = style(
        &.hover(color(c"#DB2828").important))
    }
  }

  // ===================================================================================================================

  object help {

    private val descCls = "_d"
    private val sampleCls = "_s"

    @UsesSemanticUiManually
    val examplesTable = style(
      addClassNames("ui", "celled", "table"),
      marginBottom(1 em),
      unsafeChild("tr:nth-child(odd)  td." + sampleCls)(backgroundColor(c"#fffde8")),
      unsafeChild("tr:nth-child(even) td." + sampleCls)(backgroundColor(c"#def2fc")),
      unsafeChild("tr:nth-child(odd)  td." + descCls)(backgroundColor(c"#fcf8e3")),
      unsafeChild("tr:nth-child(even) td." + descCls)(backgroundColor(c"#d9edf7")))

    val exampleDesc = style(
      addClassNames(descCls))

    val exampleDescCode = style(
      padding(0.1 em, 0.5 ex),
      margin.vertical(0.5 ex),
      fontFamily :=! "monospace", // TODO :=! ???
      backgroundColor(c"#fff"))

    val exampleSample = style(
      addClassNames(sampleCls),
      fontFamily :=! "monospace",
      whiteSpace.nowrap,
      color(c"#f39"))
  }

  // ===================================================================================================================

  initInnerObjects(
    generic.table,
    home.cardHeader,
    help.examplesTable,
    impgraphPage.graph,
    cfg.deadMnemonic,
    deletionRestorationForms.main,
    deletionForm.bottomSections,
    issues.rowspanOuter,
    restorationForm.bottomSection,
    reqtable.creation.buttonDropdown,
    reqtable.filterEditor.input(Valid),
    reqtable.sortEditor.dragArea,
    reqtable.page.viewCtrls,
    reqtable.table.selectionColumnHeader,
    reqtable.savedViews.activeItem,
    reqdetail.detailTable,
    reqdetail.useCaseStep.container,
    widgets.issueDesc,
    widgets.reqTypeSelector.dropdown)
//  ConsoleIO(_.log(render[String])).unsafePerformIO()
//  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
