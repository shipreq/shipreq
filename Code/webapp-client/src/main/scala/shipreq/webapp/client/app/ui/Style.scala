package shipreq.webapp.client.app
package ui

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import scalacss.{PseudoElement, Pseudo, StyleS}
import shipreq.base.util._
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.data.{Live, Dead}
import shipreq.webapp.client.lib.ConsoleCB
import shipreq.webapp.client.util._

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

  // ===================================================================================================================
  // Config screens
  object cfg {

    val deadMnemonic = style(
      color(c"#aaa"),
      textDecoration := ^.lineThrough)
  }

  // ===================================================================================================================
  object reqtable {
    import ui.reqtable.{Column, ColumnRenderer}

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

    val columnHeader = styleF(D.live *** D.dragStatus) { case (live, status) => styleS(
      cursor.pointer,
      deadColumnLabel(live),
      backgroundColor(c"#e0e8f8"),
      cursor.pointer, // Because click affects sorting
      (status match {
        case DragToReorder.Normal => mixin(
          border(1 px, solid, c"#777"))
        case DragToReorder.DragSource | DragToReorder.Tombstone => mixin(
          opacity(.4),
          border(2 px, dashed, c"#779"))
      }): StyleS
    )}

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
      color(c"#a00")
    )

    private val autoCompleteDesc =
      styleS(color(c"#444"), fontStyle.italic, overflow.hidden, maxWidth(36 ex))

    val reqAutoComplete = styleC {
      val r = styleS(fontWeight.bold)
      r.named('req) :*: autoCompleteDesc.named('desc)
    }

    val codeRefToReqAutoComplete = styleC {
      val code  = styleS(fontWeight.bold)
      val pubid = styleS(paddingLeft(1 ex), color(c"#333"))
      code.named('code) :*: pubid.named('pubid) :*: autoCompleteDesc.named('desc)
    }

    def codeRefToGroupAutoComplete = reqAutoComplete

    val textEditPreview = style(
      padding(h = 0.8.ex, v = 0.2.em),
      border(solid, 1 px, c"#222"),
      minHeight(2 em),
      backgroundColor(c"#efe")
    )

  } // reqtable

  // ===================================================================================================================
  object widgets {

    private def hasError = errorRedOnRed

    private val refColour = color(c"#2363A1")

    private val hoverShowsInfo = hasTitle(
      cursor.help)

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

    val blankLine = style(display.block, height(1 em))

    val ul = style(paddingLeft(2.4 ex))

    val pubidColumnValue = styleF(D.live)(a => styleS(
      display.inline,
      whiteSpace.nowrap,
      mixinIf(a :: Dead)(deadAndNotError)))

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

    val reqType = styleF(D.live)(a => styleS(
      hoverShowsInfo,
      mixinIf(a :: Dead)(deadAndNotError)))

    val issue = style(hasError)

    val issueDesc = style(
      padding.horizontal(0.7 ex))

    val reqRef = styleF(D.`live * validity`){ case (l, v) => styleS(
      mixinIf(l :: Live)(refColour),
      mixinIf(l :: Dead)(deadMaybeValid(v)),
      hoverShowsInfo
    )}

    def reqCodeGroupRef = reqRef

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
    widgets.issue)
//  ConsoleIO(_.log(render[String])).unsafePerformIO()
//  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
