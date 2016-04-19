package shipreq.webapp.client.app

import japgolly.scalajs.react.vdom.prefix_<^.{^ => ^^, _}
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import scalacss.{PseudoElement, Pseudo, StyleS}
import shipreq.base.util._
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.data.{StaticField, Live, Dead}
import shipreq.webapp.client.data._
import shipreq.webapp.client.widgets._

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

  // ===================================================================================================================
  // Config screens
  object cfg {

    val deadMnemonic = style(
      color(c"#aaa"),
      textDecoration := ^.lineThrough)
  }

  // ===================================================================================================================
  object reqtable {
    import shipreq.webapp.client.app.reqtable.{Column, ColumnRenderer}

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

    val cell = styleF(ColumnRenderer.statusDomain){ status =>
      styleS(
        border(1 px, solid, c"#ccc"),
        &.focus(
          backgroundColor(c"#e9e9ff")),
        (status match {
          case ColumnRenderer.Normal => mixin(
            padding(v = 2.px, h = 4.px))
          case ColumnRenderer.DeadRow => mixin(
            padding(v = 2.px, h = 4.px), backgroundColor(c"#eee"))
          case ColumnRenderer.`N/A` => mixin(
            padding.`0`,
            backgroundColor(c"#eee"),
            textAlign.center,
            verticalAlign.middle)
        }): StyleS
      )
    }

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

    val header = style(
      fontSize(220 %%),
      display.flex)

    val headerId = style(
      whiteSpace.pre)

    val headerTitle = style(
      marginLeft(1 ex),
      flexGrow(1))

    val mainTable = style(
      width(100 %%),
      marginTop(2 em))

    private def padSizeL = 0.8 ex

    def rowCell = styleS(
      padding.vertical(0.4 em),
      paddingLeft(padSizeL))

    val rowTitle = style(
      rowCell,
      whiteSpace.pre,
      paddingRight(1.4 ex))

    val rowValue = style(
      rowCell,
      paddingRight(padSizeL),
      width(100 %%))

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
  }

  // ===================================================================================================================

  initInnerObjects(
    cfg.deadMnemonic,
    reqtable.sortEditor.dragArea,
    reqtable.sortCriteriaEditor.conclusiveColumnName,
    reqtable.filterEditor.errorMsg,
    reqtable.table,
    reqtable.deleteRestore.impliedByItem(Live),
    reqdetail.mainTable,
    reqdetail.useCaseStep.container,
    widgets.issue)
//  ConsoleIO(_.log(render[String])).unsafePerformIO()
//  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
