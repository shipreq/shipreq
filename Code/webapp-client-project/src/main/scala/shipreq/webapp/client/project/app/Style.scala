package shipreq.webapp.client.project.app

import japgolly.scalajs.react.vdom.prefix_<^.{^ => ^^, _}
import japgolly.univeq._
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import scalacss.{Pseudo, PseudoElement, StyleS}
import shipreq.base.util._
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.data.{Dead, Live, StaticField}
import shipreq.webapp.client.base.data._
import shipreq.webapp.client.base.ui.BaseStyles
import shipreq.webapp.client.base.ui.semantic.{Colour, Label, UsesSemanticUiManually}
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

    val `live * on`       = live *** on
    val `live * validity` = live *** validity

    val ucStepIndent = Domain.ofRange(0 until StaticField.useCaseStepTrees.iterator.map(_.maxDepth).max)
  }

  /** Drag'n'drop handle Ξ */
  private val dragHnd = style(
    color(c"#000"))

  /** An empty style */
  private val empty = style()

  private val hasErrorBackground =
    backgroundColor(c"#fee")

  private val hasErrorColor = style(
    color(c"#c00"))

  private val errorRedOnRed = mixin(
    hasErrorColor,
    hasErrorBackground)

  private def deadColumnLabel(live: Live) =
    mixinIf(live :: Dead)(textDecoration := ^.lineThrough)

  private val hasTitle = Pseudo.Custom("[title]", PseudoElement)

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

  val svgGraph = style(
    unsafeChild("svg")(
      maxWidth(100 %%)))

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
      color(c"#aaa"),
      textDecoration := ^.lineThrough)
  }

  // ===================================================================================================================
  object reqtable {
    import shipreq.webapp.client.project.app.reqtable.Column
    import shipreq.webapp.client.project.app.reqtable.Table.CellStatus

    val pubidColumnValue = styleF(D.live)(a => styleS(
      display.inline,
      whiteSpace.nowrap,
      mixinIf(a :: Dead)(deadAndNotError)))

    val viewSettingsHeader = style(
      backgroundColor(c"#ffe"))

    // -----------------------------------------------------------------------------------------------------------------
    object sortCriteriaEditor {

      /** 1. Ξ [▲ Ascending] Code */
      val inconclusiveCriterionRow = styleF(D.on)(o => styleS(
//        mixinIf(!on)(
//          backgroundColor(c"#e2e2e2")),
        marginBottom(0.7 ex),
        paddingRight(1 ex)))

      def dragHnd = Style.dragHnd

      val inconclusiveSortMethod = style(
        width(28 ex))

      val inconclusiveColumnName = styleF(D.`live * on`) { case (live, on) => styleS(
        marginLeft(1 ex),
        mixinIf(on :: Off)(color(c"#999")),
        deadColumnLabel(live)
      )}

      val conclusiveSortMethod = style(
        marginLeft(4 ex))

      val conclusiveColumnName = style(
        marginLeft(1 ex))
    }

    // -----------------------------------------------------------------------------------------------------------------
    val columnsEditor =
      Live.memo(live =>
        On.memo(on => OrderedSubsetEditor.Styles(
          dragHnd = sortCriteriaEditor.dragHnd,
          label   = sortCriteriaEditor.inconclusiveColumnName(live, on))))

    // -----------------------------------------------------------------------------------------------------------------
    object filterEditor {

      val editor = styleF(D.validity)(v => styleS(
        marginTop(1.6 em),
        width(100 %%),
        height(3 em),
        mixinIf(v :: Invalid)(hasErrorBackground)
      ))

      def errorMsg = hasErrorColor
    }

    // -----------------------------------------------------------------------------------------------------------------
    object sortEditor {

      val outer = style()

      val dragArea = style(
        display.inlineBlock,
        paddingRight(12.ex), // ← Gives a bit more room to drag to tail, rather than outside
        paddingLeft(1 ex))

      val itemOuter = styleF(D.dragStatus)(status =>
        styleS(
          marginRight(2 ex),
          cursor.pointer, // Because click changes sort direction
          (status match {
            case DragToReorder.Normal => mixin(
              display.inlineBlock,
              border(1 px, solid, c"#bbb"))
            case DragToReorder.DragSource => mixin(
              display.inlineBlock,
              opacity(.4),
              border(2 px, dashed, c"#000"))
            case DragToReorder.Tombstone => mixin(
              display.none)
          }): StyleS
        ))

      val itemSortMethod = style(
        border.none,
        backgroundColor(c"#ddd"),
        verticalAlign.middle,
        textAlign.center,
        padding(v = 0.2.ex, h = 0.5.ex))

      val sortMethodFull = style(
        width(0.6.em),
        height(0.6.em))

      private val sortMethodHalf = mixin(
        display.block,
        width(0.55.em),
        height(0.55.em))

      val sortMethodHalfTop = style(
        sortMethodHalf,
        marginBottom(0.2.em))

      val sortMethodHalfBottom = style(
        sortMethodHalf)

      val itemName = styleF(Domain.boolean)(conclusive =>
        styleS(
          border.none,
          backgroundColor(c"#eee"),
          padding(v = 0.4.ex, h = 1.ex),
          verticalAlign.middle,
          mixinIf(conclusive)(fontWeight.bold)))
    }

    // -----------------------------------------------------------------------------------------------------------------

    val statsSummary = style(
      margin(1 em, `0`),
      padding(0.2 ex, 1 ex),
      color(c"#444"),
      backgroundColor(c"#ded"),
      width(100 %%))

    // http://stackoverflow.com/questions/446624/table-cell-widths-fixing-width-wrapping-truncating-long-words
    val table = style(
      marginTop(1.6 ex),
      width(100 %%))

    private val mnemonicLen =
      Grammar.reqTypeMnemonic.length.total.max

    val columnPubid   = style(maxWidth((mnemonicLen + 5).ex))
    val columnReqType = style(maxWidth(mnemonicLen.ex))

    val `N/A` = style(
      color(c"#666"),
      margin.horizontal(auto)
    )

    private val columnHeaderBase = mixin(
      backgroundColor(c"#e0e8f8"))

    val columnHeader = styleF(D.live *** D.dragStatus) { case (live, status) => styleS(
      columnHeaderBase,
      deadColumnLabel(live),
      cursor.pointer, // Because click affects sorting
      (status match {
        case DragToReorder.Normal => mixin(
          border(1 px, solid, c"#777"))
        case DragToReorder.DragSource | DragToReorder.Tombstone => mixin(
          opacity(.4),
          border(2 px, dashed, c"#779"))
      }): StyleS
    )}

    val selectionRowHeader = style(
      columnHeaderBase)

    val cell = styleF(CellStatus.domain){ status =>
      styleS(
        border(1 px, solid, c"#ccc"),
        &.focus(
          backgroundColor(c"#e9e9ff")),
        (status match {
          case CellStatus.Normal => mixin(
            padding(v = 2.px, h = 4.px))
          case CellStatus.DeadRow => mixin(
            padding(v = 2.px, h = 4.px), backgroundColor(c"#eee"))
          case CellStatus.`N/A` => mixin(
            padding.`0`,
            backgroundColor(c"#eee"),
            textAlign.center,
            verticalAlign.middle)
        }): StyleS
      )
    }

    val autoCompleteItemTitle = style(
      fontWeight.bold)

    val autoCompleteItemTitle2 = style(
      paddingLeft(1 ex),
      color(c"#333"))

    val autoCompleteItemDesc = style(
      color(c"#444"),
      fontStyle.italic,
      overflow.hidden,
      maxWidth(36 ex))

    object deleteRestore {

      val section = style(
        marginTop(2.3 em),
        marginBottom(1 em),
        fontWeight.bold)

      val row = styleF(D.live)(live => styleS(
        mixinIf(live :: Dead)(backgroundColor(c"#fee"), color(c"#a00"))
      ))

      val indent: Int => TagMod =
        Memo(n => TagMod(^^.display.`inline-block`, ^^.width := s"${n * 3}ex"))

      val reqDesc =
        style(marginLeft(0.5 ex))

      val impliedByPrefix =
        style(marginRight(0.5 ex))

      val impliedByItem = styleF(D.live)(l => styleS(
        // hoverShowsInfo, // It's a link to ReqDetail now
        mixinIf(l :: Live)(color(c"#111")),
        mixinIf(l :: Dead)(
          //textDecoration := ^.lineThrough,
          color(c"#daa"))
      ))

      def subCodeCount = impliedByItem
    }

  } // reqtable

  // ===================================================================================================================
  object reqdetail {

    val headerRow = style(
      display.flex)

    val headerPubid = style(
      paddingRight(0.4 rem))

    val headerTitle = style(
      flexGrow(1),
      paddingLeft(0.4 rem))

    val headerText = styleF(D.live)(live => styleS(
      margin(`0`).important,
      mixinIf(live :: Dead)(
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
      verticalAlign.top)

    val detailTableKey = styleF(D.live)(live => styleS(
      detailTableCell,
      textAlign.left,
      wordWrap.breakWord,
      // whiteSpace.nowrap,
      backgroundColor(
        live match {
          case Live => rgba(0, 0, 0, .04)
          case Dead => rgba(0, 0, 0, .09)
        }
      )))

    val detailTableValue = styleF(D.live)(live => styleS(
      detailTableCell,
      width(100 %%),
      mixinIf(live :: Dead)(backgroundColor(rgba(0, 0, 0, .04)))))

    val generalImpsCont = style(
      width(100 %%))

    val generalImpsSide = style(
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
        paddingLeft(0.6 ex))

      val ctrls = style(
        width(8.9 rem),
        textAlign.right)

      val ctrl = style(
        addClassNames("btn", "btn-default", "btn-sm"),
        fontSize(16 px),
        lineHeight(16 px),
        padding(4 px),
        width(26 px))
    }
  }

  // ===================================================================================================================
  object widgets {

    val richTextPreview = style(
      addClassNames("ui", "segments", "raised"))

    val richTextPreviewHeader = style(
      addClassNames("ui", "segment", "inverted", "green"),
      paddingTop(0.3 em).important,
      paddingBottom(0.3 em).important)

    val richTextPreviewBody = style(
      addClassNames("ui", "segment"),
      (backgroundImage := "repeating-linear-gradient(-225deg,rgba(0,0,0,0),rgba(0,0,0,0)5ex,rgba(33,186,67,.07)5ex,rgba(33,186,67,.07)10ex)")
        .important)

    private val refColour = color(c"#2363A1")

    val blankLine = style(display.block, height(1 em))

    val ul = style(paddingLeft(2.4 ex))

    private def tagBase(live: Live) = mixin(
      display.inlineBlock,
      mixinIf(live :: Dead)(&.not(_.hover)(textDecoration := ^.lineThrough)),
      hoverShowsInfo)

    private def tagLabelSuffix(live: Live) = live match {
      case Live => "primary"
      case Dead => "default"
    }
    val tag = styleF(D.live)(live => styleS(
      tagBase(live),
      addClassName(s"label label-${tagLabelSuffix(live)}")))

    val tagInText = styleF(D.`live * validity`){ case (l, v) => styleS(
      tagBase(l),
      mixinIf(l :: Live)(refColour),
      mixinIf(l :: Dead)(deadMaybeValid(v)))
    }

    val reqTypeShort = styleF(D.live)(a => styleS(
      hoverShowsInfo,
      mixinIf(a :: Dead)(deadAndNotError)))

    val issue = style(hasError)

    val erroneousUseCaseStepRef = style(hasError)

    val issueDesc = style(
      padding.horizontal(0.7 ex))

    val reqRef = styleF(D.`live * validity`){ case (l, v) => styleS(
      // hoverShowsInfo, // It's a link to ReqDetail now
      mixinIf(l :: Live)(refColour),
      mixinIf(l :: Dead)(deadMaybeValid(v))
    )}

    def reqCodeGroupRef = reqRef

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
      lineHeight(1 em),
      wordBreak.keepAll,
      wordWrap.normal,
      whiteSpace.pre
    )
    private val reqCodeTreePre = mixin(reqCodePre, display.inline)

    val reqCodeTreeIndent = style(reqCodeTreePre, color(c"#dadada"))
    val reqCodeTreeCode = style(reqCodeTreePre)
    val reqCodeFlat = style(reqCodePre, display.block, overflowY.hidden)

    val useCaseStepLayoutCell = style(
      border.none.important)

    object reqTypeSelector {
      val dropdown = style(
        backgroundColor(BaseStyles.editorBackgroundColor).important,
        borderColor(BaseStyles.editorBorderColor).important,
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
    home.cardHeader,
    help.examplesTable,
    impgraphPage.graph,
    cfg.deadMnemonic,
    reqtable.sortEditor.dragArea,
    reqtable.sortCriteriaEditor.conclusiveColumnName,
    reqtable.filterEditor.errorMsg,
    reqtable.table,
    reqtable.deleteRestore.impliedByItem(Live),
    reqdetail.detailTable,
    reqdetail.useCaseStep.container,
    widgets.issue,
    widgets.reqTypeSelector.dropdown)
//  ConsoleIO(_.log(render[String])).unsafePerformIO()
//  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
