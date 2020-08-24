package shipreq.webapp.client.project.app

import japgolly.scalajs.react.vdom.html_<^.{^ => ^^, _}
import scalacss.internal.ValueT
import shipreq.base.util._
import shipreq.webapp.base.CssSettings._
import shipreq.webapp.base.data.{Dead, Live, StaticField, _}
import shipreq.webapp.base.feature.DragToReorderFeature.{Status => DragStatus}
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.base.ui.semantic.UsesSemanticUiManually

object Style extends StyleSheet.Inline {
  import dsl._
  import LeftRight.{Left, Right}

  /** Domains */
  object D {
    val live      = Domain.ofValues[Live]     (Live, Dead)
    val validity  = Domain.ofValues[Validity] (Valid, Invalid)
    val enabled   = Domain.ofValues[Enabled]  (Enabled, Disabled)
    val leftRight = Domain.ofValues[LeftRight](Left, Right)

    @inline def on = BaseStyles.D.on

    val dragStatus =
      Domain.ofValues[DragStatus](DragStatus.allValues.whole: _*)

    val `enabled * live`         = enabled *** live
    val `live * live`            = live *** live
    val `live * on`              = live *** on
    val `live * validity`        = live *** validity
    val `live * validity * bool` = live *** validity *** Domain.boolean

    val ucStepIndent = Domain.ofRange(0 until StaticField.useCaseStepTrees.iterator.map(_.maxDepth).max)
  }

  final val animSpeedMs = 220
  protected final val animSpeed = animSpeedMs.toString + "ms"

  private def monospace =
    BaseStyles.monospace

  private val hasErrorBackground =
    backgroundColor(c"#fee")

  private val errorRed = c"#c00"
  private val warningYellow = c"#948a00"

  private val hasErrorColor =
    color(errorRed)

  private val errorRedOnRed = mixin(
    hasErrorColor,
    hasErrorBackground)

  private def deadColumnLabel(live: Live) =
    mixinIf(live is Dead)(textDecoration := ^.lineThrough)

  private val hasTitle = Pseudo.Custom("[title]", PseudoType.Element)

  private val hoverShowsInfo = hasTitle(cursor.help)

  private def hasError = errorRedOnRed

  private val lightLineColour = c"#e8e8e8"

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

  private val deadCell = styleS(
    backgroundColor(c"#f2f2f2"),
    color(c"#4d4d4d"),
    opacity(0.7),
  )

  val semanticFixes = style(

    // When semanticui.Select.apply is used and there isn't an option selected, this fixes it up so that the field
    // title is at the correct vertical position.
    //
    // Example: Field config > new imp field > imp field & checkbox
    unsafeRoot(".ui.dropdown>.text")(minHeight(0.8 em)),
  )

  val svgGraphFitToWidth = style(
    unsafeChild("svg")(
      maxWidth(100 %%)))

  val svgGraphError = style(
    errorRedOnRed)

  val svgGraphInvalid = style(
    opacity(0.2))

  private val selectionCellBase = style(
    width(24.px).important,
    textAlign.center.important)

  private val genericDragStatus: DragStatus => StyleS = {
    case DragStatus.Normal     => StyleS.empty
    case DragStatus.Tombstone  => mixin(display.none)
    case DragStatus.DragSource =>
      val w = 2 px
      val s = dashed
      val c = c"#000"
      styleS(
        opacity(.5),
        border(w, s, c).important,
        unsafeExt("tr" + _ + " >td")(
          borderTop(w, s, c).important,
          borderBottom(w, s, c).important,
        ),
        unsafeExt("tr" + _ + " >td:first-child")(
          borderLeft(w, s, c).important,
        ),
        unsafeExt("tr" + _ + " >td:last-child")(
          borderRight(w, s, c).important,
        ),
      )
  }

  private def genericDragStatus(ds: DragStatus, whenVisible: StyleS): StyleS =
    styleS(
      genericDragStatus(ds),
      ds match {
        case DragStatus.Normal
           | DragStatus.DragSource => whenVisible
        case DragStatus.Tombstone  => StyleS.empty
      }
    )

  val usageZero = style(&.not(_.hover)(opacity(0.5)))

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

    // On here means selected
    // N/A cells should just be specified as Dead
    def tableCellBase(i: (Live, On)) = styleS(
      i match {
        case (Live, Off) => mixin()
        case (Dead, Off) => deadCell
        case (Live, On ) => mixin(backgroundColor(hsla(228, 90 %%, 75 %%, .12)))
        case (Dead, On ) => mixin(backgroundColor(hsla(228, 74 %%, 33 %%, .11)))
      })

    val `N/A` = style(
      color(c"#666"),
      // margin.horizontal(auto),
    )

    val deadTextStrikeThrough = style(
      color(c"#999"),
      textDecoration := "line-through",
    )
  }

  object navBar {
    val connected          = style(cursor.pointer, opacity(0.7).important)
    val disconnected       = style(cursor.pointer, opacity(1).important)
    val unsavedChangesIcon = style(fontSize(1.25 em), marginTop(0.1 em))
    val unsavedChangesText = style(fontSize(1.25 em), marginRight(0.5 ex))

    private val unsavedChangesItemS = styleS(
      color(c"#ffec70").important)

    val unsavedChangesItem = style(
      unsavedChangesItemS,
      unsafeExt(s => s".ui.inverted.menu $s.active.item")(unsavedChangesItemS),
      unsafeExt(s => s".ui.inverted.menu $s.active.item:hover")(unsavedChangesItemS))
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
  object reqgraphPage {

    val container = style(
      display.flex,
      flexDirection.column,
      height(100 %%),
    )

    val noContent = style(
      marginTop(1 em))

    val graph = style(
      flexGrow(1))

    val deadDropdownItem = style(
      textDecoration := "line-through",
      color(c"#999"))

    val controlsRow2 = style(
      display.flex,
      marginTop(1.1 rem),
      marginBottom(2.3 rem))

    val configContainer = style(
      display.grid,
      gridTemplateColumns := "auto auto auto",
      gridTemplateRows := "auto auto",
      gridTemplateAreas(
        "dh lh ch",
        "de le ce",
      ),
      columnGap(1 em))

    val controlsFilter = style(
      flexGrow(1),
      marginLeft(1 rem),
      textAlign.right,
      unsafeChild("input")(textAlign.left))

    private val configHeader = mixin(
      fontWeight.bold)

    val configGraphDirHeader = style(gridArea := "dh", configHeader)
    val configGraphDirEditor = style(gridArea := "de")
    val configLabelsHeader   = style(gridArea := "lh", configHeader)
    val configLabelsEditor   = style(gridArea := "le")
    val configColoursHeader  = style(gridArea := "ch", configHeader)
    val configColoursEditor  = style(gridArea := "ce")
  }

  // ===================================================================================================================
  // Config screens
  object cfg {

    val deadMnemonic = style(
      marginTop(0.4 ex),
      color(c"#aaa"),
      textDecoration := ^.lineThrough)

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
  }

  // ===================================================================================================================
  object reqtable {

    // nearly everything here is !important because of stupid Semantic UI

    private def pageVGap = 1.25 em

    object page {

      val ctrlHGap = 1.2 ex

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
    }

    object creation {

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

      val toastLink = style(
        fontWeight.bold,
        marginLeft(0.45 ex),
      )
    }

    object table {
      def table = generic.table
      @inline private def headerBase = generic.tableHeaderBase

      val columnHeader = styleF(D.live *** D.dragStatus) { case (live, status) => styleS(
        headerBase,
        deadColumnLabel(live),
        cursor.pointer.important, // Because click affects sorting
        (status match {
          case DragStatus.Normal => mixin()
          case DragStatus.DragSource
             | DragStatus.Tombstone =>
            mixin(
              opacity(.4).important,
              border(2 px, dashed, c"#779").important)
        }): StyleS
      )}

      private val cellBase = styleF(D.`live * on`)(i => styleS(
        generic.tableDataBase,
        generic.tableCellBase(i),
      ))

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

      @inline def `N/A` = generic.`N/A`
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
        paddingRight(10.ex)) // <- Gives a bit more room to drag to tail, rather than outside

      val draggableCriterion = styleF(D.dragStatus)(status =>
        styleS(
          marginLeft(2 ex),
          borderRadius(4.px),
          cursor.pointer, // Because click changes sort direction
          // inlineFlex required below to keep the entire row at the right height
          // inlineBlock adds extra height and causes height differences between filter section & column button
          genericDragStatus(status, styleS(display.inlineFlex)),
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
  }

  // ===================================================================================================================
  object savedViews {

    val viewRow = style(display.flex)

    val viewRowSV = style(
      flexGrow(1),
      paddingRight(1.rem))

    val filterDeadButtonContainer = style(
      paddingRight(`0`).important)

    val menu = TagMod(^^.display.flex, ^^.flexWrap.wrap)

    val activeItem = style(
      fontWeight._700.important,
      (boxShadow := "none").important,
      borderColor(c"#F2711C").important,
      color(c"#F2711C").important)

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
      paddingLeft(0.4 rem),
      &.focus(BaseStyles.focus.glowOutline))

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

    val reqTypeRow  = style(display.flex)
    val reqTypeRowL = style(flexGrow(1))
    val reqTypeRowR = style(background := "#fff", marginLeft(0.6 rem))

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

      @inline
      @nowarn("cat=unused")
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

    @inline val na = generic.`N/A`

    val actionButton = style(
      textAlign.left.important,
      whiteSpace.nowrap.important,
      color(c"#fff").important,
      (background := "#2f9d3e").important,
      padding(0.4 em, 0.8 em).important)
  }

  // ===================================================================================================================
  object configShared {

    sealed trait RowState
    object RowState {
      case object Disabled extends RowState
      case object Enabled  extends RowState
      case object Selected extends RowState
      case object Dragging extends RowState
      case object ReadOnly extends RowState

      implicit def univEq: UnivEq[RowState] = UnivEq.derive

      val domain: Domain[RowState] =
        Domain.ofValues(Disabled, Enabled, Selected, Dragging, ReadOnly)

      val withDragStatus =
        domain *** D.dragStatus

      val withDragStatusAndLive =
        withDragStatus *** D.live

      val withLive =
        domain *** D.live
    }

    def crudRow(rowState: RowState,
                dragStatus: DragStatus,
                bottomBorderColour: ValueT[ValueT.Color] = c"#fff") = mixin(
      mixinIf(dragStatus != DragStatus.DragSource)(
        borderTop(solid, 1 px, if (rowState ==* RowState.Selected) c"#3659e2" else c"#fff"),
        borderBottom(solid, 1 px, if (rowState ==* RowState.Selected) c"#3659e2" else bottomBorderColour).important,
        unsafeExt("tr" + _ + ">td")(
          mixinIf(rowState ==* RowState.Selected)(
            borderTop(solid, 1 px, c"#3659e2").important,
            borderBottom(solid, 1 px, c"#3659e2").important,
          ),
        ),
      ),
      rowState match {
        case RowState.Enabled  => styleS(
          cursor.pointer,
          &.hover(backgroundColor(c"#fffad7").important),
        )
        case RowState.Selected => styleS(
          backgroundColor(Color("#869df91f")),
        )
        case RowState.ReadOnly => styleS(
          mixinIf(dragStatus != DragStatus.DragSource)(opacity(0.7)),
        )
        case RowState.Dragging
           | RowState.Disabled =>
          StyleS.empty
      }
    )
  }

  // ===================================================================================================================
  object fieldConfig {
    type RowState = configShared.RowState
    @inline def RowState = configShared.RowState

    val fieldListTable = style(
      addClassNames("ui", "celled", "table")
    )

    val fieldListTableRow = styleF(RowState.withDragStatusAndLive) { case ((s, ds), l) => styleS(
      genericDragStatus(ds),
      configShared.crudRow(s, ds),
      mixinIf(l is Dead)(
        unsafeExt("tr" + _ + ">td")(deadCell),
      ),
    )}

    val fieldListTableName = styleF(D.live)(l => styleS(
      generic.tableCellBase((l, Off)),
      mixinIf(l is Dead)(textDecoration := "line-through"),
    ))

    val fieldListTableCell = styleF(D.live)(l => styleS(
      generic.tableCellBase((l, Off)),
    ))

    val fieldListTableUsage = styleF(D.live)(l => styleS(
      generic.tableCellBase((l, Off)),
      textAlign.right.important,
    ))

    val fieldListTableDrag = styleF(D.live)(l => styleS(
      mixinIf(l is Dead)(deadCell),
      padding(`0`).important,
      textAlign.center.important,
    ))

    val dragHandle = styleF(D.`enabled * live`) { case (e, l) => styleS(
      display.inline,
      padding(.5 em, 2 ex),
      mixinIf(e is Enabled)(cursor.grab),
      mixinIf(e.is(Disabled) && l.is(Dead))(visibility.hidden),
    )}

    val detailRule = styleF(D.validity)(v => styleS(
      lineHeight(1.8 em),
      mixinIf(v is Invalid)(errorRedOnRed),
    ))

    val detailRuleKey = style(
      fontWeight.bold,
    )

    val detailRuleSep = style(
      margin.horizontal(.7 ex)
    )

    val fieldListDetailNoOtherTags = style(
      color(c"#999"),
    )

    val fieldListDetailOtherTags = style(
      marginTop(0.2 em),
    )

    val fieldListDetailDerivativeTagsIcon = style(
      marginLeft(`0`),
      marginRight(.7 ex))

    val rulesEditor = style(
      addClassNames("table", "ui", "single", "line", "table")
    )

    private val rulesEditorReqTypeColumn = styleS(
      width(44 ex)
    )

    val rulesEditorReqTypes = style(
      rulesEditorReqTypeColumn,
      unsafeChild(".ui.input")(width(100 %%)),
    )

    val rulesEditorRule = style(
    )

    val rulesEditorDefault = style(
      marginLeft(1.5 ex),
    )

    val rulesEditorOtherwise = style(
      rulesEditorReqTypeColumn,
      paddingLeft(2 ex).important,
    )

    val rulesEditorButton = style(
      textAlign.right,
      width(1 px),
    )

    val rulesDeadReqTypes = style(
      rulesEditorReqTypeColumn,
      color(c"#444"),
      paddingLeft(2 ex).important,
    )

    val rulesDeadReqTypesInner = style(
      color(c"#999"),
      marginLeft(1 ex),
    )

    val staticFieldUL = style(
      color(c"#3f3f3f"),
      margin.vertical(3 em),
      fontSize(1.1 rem),
    )

    val staticFieldLI = style(
      marginBottom(0.7 em),
    )

    val staticFieldTagLI = style(
      marginTop(0.3 em),
    )

    @inline def `N/A` = generic.`N/A`
    @inline def editorTitle = tagConfig.editorTitle
    @inline def fieldListDetailDead = rulesOtherDeadReqType
    @inline def rulesOtherDeadReqType = generic.deadTextStrikeThrough

    val derivativeTagMatrixSame = style(
      opacity(0.5),
      backgroundColor(c"#eee"))

    val derivativeTagMatrixNone = style(
      color(c"#bbb"))

    val derivativeTagsEditorContainer = style(
      marginTop(1 em))

    val derivativeTagsEditor = styleF(D.validity)(validity => styleS(
      monospace,
      mixinIf(validity is Invalid)(borderColor(errorRed)),
    ))

    val derivativeTagsEditorWarningBody = style(
      color(warningYellow))

    val derivativeTagsEditorErrorBody = style(
      color(errorRed))

    val derivativeTagsEditorWarningTitle = style(
      derivativeTagsEditorWarningBody,
      fontWeight.bold)

    val derivativeTagsEditorErrorTitle = style(
      derivativeTagsEditorErrorBody,
      fontWeight.bold)
  }

  // ===================================================================================================================
  object issueConfig {

    val sectionTitle = style(
        display.block,
        margin(1.rem, auto, 4.rem, auto),
        maxWidth(60.ex),
        fontSize(120 %%),
        textAlign.center,
        fontWeight.bold,
        borderTop(solid, 1.px, c"#ddd"),
        borderBottom(solid, 1.px, c"#ddd"),
        padding(.7 em, `0`),
    )

    val otherSources = style(
      display.flex,
      justifyContent.center,
    )

    val otherSourcesGap = style(
      width(6 rem),
    )

    val otherSourcesHeader = style(
      fontWeight.bold,
      marginBottom(1 em),
    )

    val otherSourcesContent = style(
      marginBottom(4 em),
    )

    val otherSourcesUL = style(
      listStylePosition.inside,
      paddingLeft(2 px),
    )

    val otherSourcesLI = style(
      marginBottom(.3 em),
      color(c"#444")
    )

    val otherSourcesNone = style(
      color(c"#444")
    )

    val otherSourcesSubtext = style(
      marginLeft(1 ex),
      color(c"#999"),
    )

    type RowState = configShared.RowState
    @inline def RowState = configShared.RowState
    @inline def editorTitle = tagConfig.editorTitle
    @inline def listTable      = fieldConfig.fieldListTable
    @inline def listTableCell  = fieldConfig.fieldListTableCell
    @inline def listTableRow   = reqTypeConfig.listTableRow
    @inline def listTableUsage = fieldConfig.fieldListTableUsage
  }

  // ===================================================================================================================
  object reqTypeConfig {
    type RowState = configShared.RowState
    @inline def RowState = configShared.RowState

    val listTableRow = styleF(RowState.withLive) { case (s, l) => styleS(
      configShared.crudRow(s, DragStatus.Normal),
      mixinIf(l is Dead)(
        unsafeExt("tr" + _ + ">td")(deadCell),
      ),
    )}

    val implicationHelp = style(
      marginLeft(0.45 ex).important,
      cursor.help,
    )

    val editorMnemonic = style(
      display.block,
      width(30 ex).important,
    )

    val editorPastMnemonics = style(
      marginTop(0.65 em),
    )

    val preEditorMessage = style(
      marginBottom(2 rem),
    )

    val notInUseBody = style(
      lineHeight(1.4285 em),
      opacity(0.85),
      margin(.75 em, `0`),
      unsafeChild("ol")(margin(.35 em, `0`)),
    )

    @inline def staticReadOnly = preEditorMessage
    @inline def notInUse       = preEditorMessage
    @inline def listTable      = fieldConfig.fieldListTable
    @inline def listTableCell  = fieldConfig.fieldListTableCell
    @inline def listTableUsage = fieldConfig.fieldListTableUsage
    @inline def deadMnemonic   = generic.deadTextStrikeThrough
    @inline def editorTitle    = tagConfig.editorTitle
  }

  // ===================================================================================================================
  object tagConfig {
    type RowState = configShared.RowState
    @inline def RowState = configShared.RowState

    sealed trait LIState {
      val topLevel: Boolean
    }

    object LIState {
      final case class Group(topLevel: Boolean)                                               extends LIState
      final case class Tag  (rowState: RowState, topLevel: Boolean, firstAfterGroup: Boolean) extends LIState

      implicit def univEq: UnivEq[LIState] = UnivEq.derive

      import Domain.{boolean => B}

      val domain: Domain[LIState] =
        Domain.ofValues(
          B.map(Group).iterator.toSeq ++
          (RowState.domain *** B *** B).map{case ((a, b), c) => Tag(a, b, c)}.iterator.toSeq
          : _*
        )

      val withDragStatus =
        domain *** D.dragStatus
    }

    val tagTree = style(
      margin(`0`),
      paddingLeft(`0`),
    )

    val tagSubTree = style(
      paddingLeft(6.5 ex),
    )

    private val borderColor = Color("#2224261a")

    private def liGapTop(ds: DragStatus) = mixin(
      marginTop(1.25 em),
      mixinIf(ds ==* DragStatus.Normal)(borderTop(solid, 1 px, borderColor)),
    )

    private def basicTagLI(s: LIState, ds: DragStatus) = mixin(
      listStyleType := "none",
      genericDragStatus(ds),
      s match {
        case _: LIState.Group => StyleS.empty
        case _: LIState.Tag =>
          styleS(
            paddingTop(.5 em),
            paddingBottom(.5 em),
          )
      },
    )

    private def basicTagOrGroup(rowState: RowState, ds: DragStatus) = mixin(
      transition := s"all $animSpeed",
      mixinIf(rowState ==* RowState.Disabled && ds ==* DragStatus.Normal)(opacity(0.6)),
    )

    private def tagOrGroup(rowState: RowState,
                           dragStatus: DragStatus,
                           bottomBorderColour: ValueT[ValueT.Color] = c"#fff") =
      configShared.crudRow(rowState, dragStatus, bottomBorderColour)

    val tagTreeLI = styleF(LIState.withDragStatus) { case (s, ds) => styleS(
      basicTagLI(s, ds),
      s match {
        case g: LIState.Group =>
          styleS(
            mixinIf(g.topLevel)(&.not(_.firstChild)(liGapTop(ds))),
          )
        case t: LIState.Tag =>
          styleS(
            mixinIf(t.topLevel && t.firstAfterGroup)(liGapTop(ds)),
            &.not(_.lastChild)(tagOrGroup(t.rowState, ds, borderColor)),
            &.lastChild(tagOrGroup(t.rowState, ds)),
          )
      },
    )}

    val editorRelTagLI = styleF(LIState.withDragStatus) { case (s, ds) => styleS(
      basicTagLI(s, ds),
      s match {
        case t: LIState.Tag   => basicTagOrGroup(t.rowState, ds)
        case _: LIState.Group => StyleS.empty
      },
      clear.both,
    )}

    private def basicTagGroup = mixin(
      padding(0.4 em, `0`),
    )

    val editorRelGroup = styleF(RowState.domain)(s => styleS(
      basicTagGroup,
      basicTagOrGroup(s, DragStatus.Normal),
    ))

    val tagTreeGroup = styleF(RowState.domain)(s => styleS(
      basicTagGroup,
      tagOrGroup(s, DragStatus.Normal),
      fontSize(120 %%),
    ))

    val tagTreeGroupIcon = style(
      marginRight(0.4 ex).important,
    )

    val dragHandle = styleF(D.`enabled * live`) { case (e, l) => styleS(
      display.inline,
      padding(.5 em, 1 ex, 0.5 em, 2 ex),
      marginLeft(-2 ex),
      mixinIf(e is Enabled)(cursor.grab),
      mixinIf(e.is(Disabled) && l.is(Dead))(visibility.hidden),
    )}

    val editorTitle = style(
      color(c"#3659e2"),
      marginBottom(3 rem),
    )

    val segmentCheckboxSubtitle = style(
      marginTop(0.5 em),
      opacity(0.55),
    )

    val editorButtons = style(
      marginTop(2 rem),
      display.flex,
      unsafeChild("button:not(:last-child)")(marginRight(2 em).important),
    )

    val editorButtonGap = style(
      flexGrow(1),
    )

    val editorRelRow = style(
      display.flex,
      marginTop(1 rem),
    )

    val editorRelOuter = styleF(D.leftRight)(lr => styleS(
      width(50 %%),
      lr match {
        case Left  => styleS(paddingRight(0.5 rem))
        case Right => styleS(paddingLeft(0.5 rem))
      },
    ))

    val editorRelInner = style(
      display.flex,
      flexDirection.column,
      height(100 %%),
    )

    val editorRelHeader = styleF(D.enabled)(e => styleS(
      marginBottom(1 em),
      mixinIf(e is Disabled)(opacity(0.6)),
    ))

    val editorRelBody = style(
      flexGrow(1),
    )

    val editorRelFooter = style(
      marginTop(2 em),
      unsafeChild(".ui.dropdown .menu>.item")( // fuck you Semantic UI
        paddingTop(0.4 em).important,
        paddingBottom(0.4 em).important,
      ),
    )

    val editorRelDelete = style(
      margin(-0.5 em, `0`, .5 em, `0`).important,
      float.right,
    )

    val editorRelTree = style(
      margin(t = 0.25 em, r = `0`, b = `0`, l = 2 ex),
      paddingLeft(`0`),
    )

    val group = styleF(D.live)(l => styleS(
      mixinIf(l is Dead)(
        color(c"#999"),
        textDecoration := "line-through",
      )
    ))

    val editorApTagHeader = style(
      fontSize(100 %%).important,
    )

    val usage = style(float.right)
  }

  // ===================================================================================================================
  object widgets {

    val richCodeBlockError = style(
      backgroundColor(c"#ddd"),
      padding(1 ex, 2 ex),
    )

    val richCodeBlockErrorCode = style(
      color(c"#c00"),
    )

    val richCodeBlockErrorUL = style(
      marginTop(.5 em),
      marginBottom(.2 em),
    )

    private val noDropdownError = styleS(
      (color :=! "#0009").important,
      (background := "#e8e8e8").important,
      (borderColor :=! "#0000").important,
    )

    val applicableReqTypesDropdown = style(
      noDropdownError,
      unsafeChild(".text")(noDropdownError)
    )

    val applicableReqTypesEditorFooter = style(
      marginBottom(0.15 em),
    )

    val applicableReqTypesEditorDeadRow = style(
      float.right,
      color(c"#444"),
    )

    val applicableReqTypesEditorDeadReqTypes = style(
      marginLeft(1 ex),
      color(c"#999"),
    )

    val applicableReqTypesErrMsg = style(
      color(rgb(159, 58, 56)),
      paddingTop(0.15 em),
      fontSize(92 %%),
    )

    val colourPicker = style(
      width(20 ex),
    )

    val colourPickerPickler = style(
      marginTop(0.6 em),
      unsafeChild("input:not([type])")(
        backgroundColor(c"#fff").important,
        padding(1 px, `0`).important,
      ),
    )

    private val refColour = color(c"#2363A1")

    private def blankLineHeight = 0.8 em

    val blankLine = style(display.block, height(blankLineHeight))

    private def ul = styleS(
      paddingLeft(3.2 ex),
      &.firstChild(marginTop(`0`)),
      &.lastChild(marginBottom(`0`)),
    )

    val ulCompact = style(
      ul,
      margin(blankLineHeight, `0`),
    )

    val ulSpacious = style(
      ul,
      margin(blankLineHeight, `0`, `0`, `0`),
      unsafeChild(">li")(marginBottom(blankLineHeight)),
      &.lastChild(
        unsafeChild(">li")(
          &.lastChild(marginBottom(`0`))
        ),
      ),
    )

    private def heading = style(
      marginBottom(0.3 em),
      unsafeExt(self => s"$self + $self")(
        marginTop(-0.3 em),
      )
    )

    val h1 = heading
    val h2 = heading
    val h3 = heading
    val h4 = heading
    val h5 = heading
    val h6 = heading

    val strikethrough = style(textDecoration := "line-through")

    val underline = style(textDecoration := "underline")

    val monospace = style(
      display.inline,
      padding(.2 em, .4 em),                                                  // matching github
      borderRadius(3 px),                                                     // matching github
      background := "#f5f2f0",                                                // matching prism.js
      fontFamily :=! "Consolas,Monaco,'Andale Mono','Ubuntu Mono',monospace", // matching prism.js
      fontSize(1 em),                                                         // matching prism.js
    )

    val blankButMandatory = style(
      errorRedOnRed,
      textAlign.center,
      height(100 %%),
      width(11 ex),
      borderRadius(0.3 ex))

    private def tagBase(live: Live, helpIconOnHover: Boolean) = mixin(
      mixinIf(live is Dead)(&.not(_.hover)(textDecoration := ^.lineThrough)),
      if (helpIconOnHover)
        styleS(
          hoverShowsInfo,
//          &.not(hasTitle)(cursor.default),
        )
      else
        styleS(
//          cursor.default,
        ),
    )

    private val tagLabelColour: Live => String = {
      case Live => ""
      case Dead => "grey"
    }

    @UsesSemanticUiManually
    val tag = styleF(D.`live * validity * bool`) { case ((live, validity), helpIconOnHover) => styleS(
      tagBase(live, helpIconOnHover = helpIconOnHover),
      padding(4 px, 6 px).important,
      mixinIf(validity is Invalid)(hasErrorBackground.important, hasErrorColor.important, textDecoration := ^.lineThrough),
      addClassName(s"ui label ${tagLabelColour(live)}"),
    )}

    val tagInText = styleF(D.`live * validity`){ case (l, v) => styleS(
      tagBase(l, helpIconOnHover = true),
      (l, v) match {
        case (Live, Valid)   => styleS(refColour)
        case (Live, Invalid) => styleS(hasError, textDecoration := ^.lineThrough)
        case (Dead, _)       => deadMaybeValid(v)
      },
    )}

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
      whiteSpace.nowrap,
      unsafeExt("span" + _)(hoverShowsInfo), // usually tag is <a> which links to ReqDetail
      mixinIf(l is Live)(refColour),
      mixinIf(l is Dead)(deadMaybeValid(v))
    )}

    def codeGroupRef = reqRef

    def useCaseStepRef = reqRef

    val katex = style(margin.horizontal(0.8 ex))
    val katexFail = style(katex, hasError)

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

    val dropdownButtonOuter = style(
      marginRight(`0`).important)

    val dropdownButtonGreenDropdown = styleF(Domain.boolean)(basic =>
      if (basic)
        styleS(
          paddingLeft(2.6 ex).important,
          color(c"#21ba45").important,
          outlineColor(c"#21ba45").important,
          borderColor(c"#21ba45").important,
        )
      else
        styleS(
          color(c"#eee").important,
          backgroundColor(c"#00a632").important,
        )
    )

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

    object splitScreen {
      @inline private def gap = 2 rem

      val maxBodyWidth = "100vh - 6rem"

      val outer = style(
        minHeight :=! s"calc($maxBodyWidth)",
        display.flex,
      )

      val left = style(
        width(50 %%),
        paddingRight(gap),
        borderRight(solid, 1 px, lightLineColour),
      )

      val right = style(
        width(50 %%),
        paddingLeft(gap),
      )
    }

    object splitScreenCrud {
      import splitScreen.maxBodyWidth

      val emptyRight = style(
        display.flex,
        flexDirection.column,
      )

      val emptyRightHeader = style(
        height :=! s"calc(($maxBodyWidth)/4)",
      )

      val rightOn = style(
        height.auto,
        opacity(1),
        transition := ("height 0s 0s, opacity " + animSpeed + " 0s"),
      )

      val rightOff = style(
        overflow.hidden,
        height(`0`),
        opacity(0),
        transition := ("height 0s " + animSpeed + ", opacity " + animSpeed + " 0s"),
      )

      val emptyRightBody = style(
        textAlign.center,
        fontStyle.italic,
        color(c"#999"),
        lineHeight(1.8 em),
        fontSize(120 %%),
      )

      val topLeft = style(
        display.flex,
        marginBottom(2 rem),
      )

      val topLeftGrow = style(
        flexGrow(1),
      )
    }
  }

  // ===================================================================================================================

  object help {

    @UsesSemanticUiManually
    val table = style(
      addClassNames("ui", "celled", "table"),
      display.block,
      maxHeight :=! "calc(100vh - 7rem - 3rem - 3.93rem)", // 7=modal margin, 3.93=modal header, 3=modal body
      overflowY.auto,
    )

    val groupHeader = styleF(D.enabled)(enabled => styleS(
      fontSize(115 %%),
      fontWeight.bold,
      backgroundColor(
        enabled match {
          case Enabled  => c"#222"
          case Disabled => c"#888"
        }
      ),
      color(c"#fff"),
    ))

    val groupNA = style(
      marginLeft(1 ex),
      opacity(0.6))

    val cellNA = style(
      backgroundColor(c"#ddd"),
      opacity(0.85))

    val rowText = style(
      background := "#f0fbff",
      verticalAlign.top,
    )

    val rowExamples = style(
      rowText,
      monospace,
      whiteSpace.nowrap,
      color(c"#f39"))

    val code = style(
      padding(0.1 em, 0.5 ex),
      margin.vertical(0.5 ex),
      monospace,
      backgroundColor(c"#fff"))
  }

  // ===================================================================================================================

  initInnerObjects(
    generic.table,
    navBar.connected,
    home.cardHeader,
    help.table,
    reqgraphPage.graph,
    cfg.deadMnemonic,
    deletionRestorationForms.main,
    deletionForm.bottomSections,
    issues.rowspanOuter,
    issueConfig.sectionTitle,
    restorationForm.bottomSection,
    reqtable.creation.formOuter,
    reqtable.filterEditor.input(Valid),
    reqtable.sortEditor.dragArea,
    reqtable.page.viewCtrls,
    reqtable.table.selectionColumnHeader,
    reqdetail.detailTable,
    reqdetail.useCaseStep.container,
    fieldConfig.fieldListTable,
    reqTypeConfig.implicationHelp,
    savedViews.activeItem,
    tagConfig.tagTree,
    widgets.issueDesc,
    widgets.reqTypeSelector.dropdown,
    widgets.splitScreen.left,
    widgets.splitScreenCrud.emptyRight,
  )
//  ConsoleIO(_.log(render[String])).unsafePerformIO()
//  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
