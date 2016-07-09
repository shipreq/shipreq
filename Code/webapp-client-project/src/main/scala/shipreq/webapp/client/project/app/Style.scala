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
import shipreq.webapp.client.base.ui.BaseStyles.pageMargin
import shipreq.webapp.client.base.ui.semantic.{Colour, Label}
import shipreq.webapp.client.project.widgets._

object Style extends StyleSheet.Inline {
  import dsl._

  sealed abstract class EditorState
  object EditorState {
    case object Valid     extends EditorState
    case object Invalid   extends EditorState
    case object InTransit extends EditorState

    implicit def univEq: UnivEq[EditorState] = UnivEq.derive

    implicit def fromValidity(v: Validity): EditorState =
      v match {
        case shipreq.base.util.Valid   => Valid
        case shipreq.base.util.Invalid => Invalid
      }
  }

  /** Domains */
  object D {
    val live     = Domain.ofValues[Live]    (Live, Dead)
    val validity = Domain.ofValues[Validity](Valid, Invalid)
    val enabled  = Domain.ofValues[Enabled] (Enabled, Disabled)
    val on       = Domain.ofValues[On]      (On, Off)

    val editorState = Domain.ofValues[EditorState](
      EditorState.Valid,
      EditorState.Invalid,
      EditorState.InTransit)

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

    // TODO This has  been replaced by textEditor right?
    val cellEditor = styleF(D.validity)(v => styleS(
//      borderRadius(4 px),
      width(100 %%),
//      boxShadow := "inset 0 1px 1px rgba(0,0,0,.075)",
//      transition := "border-color ease-in-out .15s, box-shadow ease-in-out .15s",
      //border(1 px, solid, if (hasError) Color(c"#a94442") else Color(c"#666")),
//      outlineColor(if (hasError) Color(c"#a94442") else Color(c"#666")),
      mixinIf(v :: Invalid)(hasErrorBackground, &.focus(outlineColor(c"#f88"))),
      padding.horizontal(0.8 ex)
    ))

    val cellEditorErrMsg = style(
      color(c"#a00"))

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

    // TODO deprecate
    val textEditPreview = style(
      padding(h = 0.8.ex, v = 0.2.em),
      border(solid, 1 px, c"#222"),
      minHeight(2 em),
      backgroundColor(c"#efe"))

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
      paddingLeft(pageMargin))

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
      display.flex,
      alignItems.center,
      width(100 %%))

    val generalImpsSide = style(
      border(^.dashed, 1 px),
      minHeight(1.59 em),
      flexBasis := "0",
      flexGrow(1))

    val generalImpsMiddle = style(
      margin.horizontal(1 ex))

    object useCaseStep {

      val container = style(
        display.flex,
        justifyContent.flexEnd, // So that controls in tail-step rows appear on the right.
        width(100 %%))

      val header = styleF(D.ucStepIndent)(lvl =>
        styleS(
          paddingTop(4 px),
          paddingRight(0.8 ex),
          color(c"#444"),
          lvl match {
            case 0 => styleS(fontWeight.bold,    width(5 ex)) // 123.0
            case 1 => styleS(paddingLeft( 4 ex), width(3 ex)) // 99.
            case 2 => styleS(paddingLeft( 7 ex), width(3 ex)) // cv.
            case 3 => styleS(paddingLeft(10 ex), width(4 ex)) // xviii.
            case 4 => styleS(paddingLeft(14 ex), width(3 ex)) // 99.
          }
        )
      )

      val deadStepLabel = style(
        color(c"#bbb"),
        textDecoration := ^.lineThrough)

      val body = style(
        flexGrow(1))

      val ctrls = style(
        width(116 px))

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

    val textEditor = styleF(D.editorState) { state =>
      styleS(
        width(100 %%),
        margin(`0`),
        padding(.3 em,.4 em),
        outlineStyle.none,
        boxShadow := "0 0 0 0 rgba(0, 0, 0, 0) inset",
        transition := "color .1s ease,border-color .1s ease",
        fontSize(1 em),
        lineHeight(1.2857),
        // overflow: scroll - autosize avoids this
        resize.none,
        color(state match {
          case EditorState.Valid
             | EditorState.InTransit => rgba(0, 0, 0, .87)
          case EditorState.Invalid   => c"#9F3A38"
        }),
        backgroundColor(state match {
          case EditorState.Valid     => c"#fff4e3"
          case EditorState.Invalid   => c"#FFF6F6"
          case EditorState.InTransit => rgba(255,244,227,0.7)
        }),
        borderWidth(1 px),
        borderStyle(state match {
          case EditorState.Valid
             | EditorState.Invalid   => solid
          case EditorState.InTransit => dashed
        }),
        borderRadius(.28571429 rem),
        borderColor(state match {
          case EditorState.Valid
             | EditorState.InTransit => rgba(255, 166, 34, .5)
          case EditorState.Invalid   => c"#E0B4B4"
        }),
        mixinIf(state ==* EditorState.InTransit)(display.flex),
        &.focus(
          (state match {
            case EditorState.Valid     => styleS(borderColor(rgb(255, 166, 34)), boxShadow := "0 0 1ex rgba(255,166,34,0.5)")
            case EditorState.Invalid   => styleS(boxShadow := "0 0 1ex rgba(224,180,180,.5)")
            case EditorState.InTransit => styleS()
          }): StyleS
        )
      )
    }

    val textEditorInTransitValue = style(
      flexGrow(1),
      opacity(0.5))

    val errorPointingUp = Label.Style(Label.Type.PointingUp, Colour.Red).div

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
    val reqCodeFlat = style(reqCodePre, display.block)

    val useCaseStepLayoutCell = style(
      border.none.important)
  }

  // ===================================================================================================================

  initInnerObjects(
    home.cardHeader,
    impgraphPage.graph,
    cfg.deadMnemonic,
    reqtable.sortEditor.dragArea,
    reqtable.sortCriteriaEditor.conclusiveColumnName,
    reqtable.filterEditor.errorMsg,
    reqtable.table,
    reqtable.deleteRestore.impliedByItem(Live),
    reqdetail.detailTable,
    reqdetail.useCaseStep.container,
    widgets.issue)
//  ConsoleIO(_.log(render[String])).unsafePerformIO()
//  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
