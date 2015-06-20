package shipreq.webapp.client.app
package ui

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import scalacss.StyleS
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.data.{Live, Dead}
import shipreq.webapp.client.lib.ConsoleIO
import shipreq.webapp.client.util._

object Style extends StyleSheet.Inline {
  import dsl._

  object D {
    val live     = Domain.ofValues[Live]    (Live, Dead)
    val validity = Domain.ofValues[Validity](Valid, Invalid)
    val enabled  = Domain.ofValues[Enabled] (Enabled, Disabled)
    val on       = Domain.ofValues[On]      (On, Off)

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

    val columnHeader = styleF(D.live)(live => styleS(
      deadColumnLabel(live),
      backgroundColor(c"#e0e8f8"),
      border(1 px, solid, c"#777")
    ))

    val cell = styleF[(ColumnRenderer.Status, Boolean)](ColumnRenderer.statusDomain *** Domain.boolean){
      case (status, focus) => styleS(
        border(1 px, solid, c"#ccc"),
        mixinIf(focus)(
          backgroundColor(c"#e9e9ff"),
          outline(rgba(0, 0, 200, 0.2), 2 px, solid),
          outlineOffset(-1 px)
        ),
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

    private val hoverShowsInfo = mixin(
      display.inlineBlock,
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
      display.inlineBlock,
      mixinIf(a :: Dead)(deadAndNotError)))

    private def tagLabelSuffix(live: Live) = live match {
      case Live => "primary"
      case Dead => "default"
    }
    val tag = styleF(D.live)(live => styleS(
      addClassName(s"label label-${tagLabelSuffix(live)}"),
      mixinIf(live :: Dead)(&.not(_.hover)(textDecoration := ^.lineThrough)),
      hoverShowsInfo))

    val reqType = styleF(D.live)(a => styleS(
      hoverShowsInfo,
      mixinIf(a :: Dead)(deadAndNotError)))

    val issue = style(hasError)

    val issueDesc = style(
      padding.horizontal(0.7 ex))

    val reqRef = styleF(D.`live * validity`){ case (a, v) => styleS(
      mixinIf(a :: Live)(color(c"#2363A1")),
      mixinIf(a :: Dead)(deadMaybeValid(v)),
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
    reqtable.sortCriteriaEditor.conclusiveColumnName,
    reqtable.filterEditor.errorMsg,
    reqtable.table,
    widgets.issue)
//  ConsoleIO(_.log(render[String])).unsafePerformIO()
//  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
