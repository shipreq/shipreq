package shipreq.webapp.client.app
package ui

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import scalacss.StyleS
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.data.{Alive, Dead}
import shipreq.webapp.client.lib.ConsoleIO
import shipreq.webapp.client.util._

object Style extends StyleSheet.Inline {
  import dsl._

  object D {
    val alive    = Domain.ofValues[Alive]   (Alive, Dead)
    val validity = Domain.ofValues[Validity](Valid, Invalid)
    val enabled  = Domain.ofValues[Enabled] (Enabled, Disabled)
    val on       = Domain.ofValues[On]      (On, Off)
  }

  /** Drag'n'drop handle Ξ */
  private val dragHnd = style(
    color("#000"))

  /** An empty style */
  private val empty = style()

  private val hasErrorBackground =
    backgroundColor("#fee")

  // ===================================================================================================================
  object reqtable {
    import ui.reqtable.{Column, ColumnRenderer}

    // -----------------------------------------------------------------------------------------------------------------
    object sortCriteriaEditor {

      /** 1. Ξ [▲ Ascending] Code */
      val inconclusiveCriterionRow = styleF(D.on)(o => styleS(
//        mixinIf(!on)(
//          backgroundColor("#e2e2e2")),
        marginBottom(0.7 ex),
        paddingRight(1 ex)))

      def dragHnd = Style.dragHnd

      val inconclusiveSortMethod = style(
        width(28 ex))

      val inconclusiveColumnName = styleF(D.on)(o => styleS(
        marginLeft(1 ex),
        mixinIf(o :: Off)(
          //textDecoration := ^.lineThrough,
          color("#999"))))

      val conclusiveSortMethod = style(
        marginLeft(4 ex))

      val conclusiveColumnName = style(
        marginLeft(1 ex))
    }

    // -----------------------------------------------------------------------------------------------------------------
    val columnsEditor =
      On.memo(on => OrderedSubsetEditor.Styles(
        dragHnd = sortCriteriaEditor.dragHnd,
        label   = sortCriteriaEditor.inconclusiveColumnName(on)))

    // -----------------------------------------------------------------------------------------------------------------

    // http://stackoverflow.com/questions/446624/table-cell-widths-fixing-width-wrapping-truncating-long-words
    val table = style(
      marginTop(1.6 ex),
      width(100 %%))

    private val mnemonicLen =
      Grammar.reqTypeMnemonic.length.total.max

    val columnPubid   = style(maxWidth((mnemonicLen + 5).ex))
    val columnReqType = style(maxWidth(mnemonicLen.ex))

    val `N/A` = style(
      color("#666".color),
      margin.horizontal(auto)
    )

    val columnHeader = style(
      backgroundColor("#ddd".color),
      border(1 px, solid, "#777".color)
    )

    val cell = styleF[(ColumnRenderer.Status, Boolean)](ColumnRenderer.statusDomain *** Domain.boolean){
      case (status, focus) => styleS(
        border(1 px, solid, "#ccc".color),
        mixinIf(focus)(
          backgroundColor("#e9e9ff"),
          outline(rgba(0, 0, 200, 0.2), 2 px, solid),
          outlineOffset(-1 px)
        ),
        (status match {
          case ColumnRenderer.Normal => mixin(
            padding(v = 2.px, h = 4.px))
          case ColumnRenderer.DeadRow => mixin(
            padding(v = 2.px, h = 4.px), backgroundColor("#eee"))
          case ColumnRenderer.`N/A` => mixin(
            padding.`0`,
            backgroundColor("#eee"),
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
      //border(1 px, solid, if (hasError) Color("#a94442") else Color("#666")),
//      outlineColor(if (hasError) Color("#a94442") else Color("#666")),
      mixinIf(v :: Invalid)(hasErrorBackground, &.focus(outlineColor("#f88"))),
      padding.horizontal(0.8 ex)
    ))

    val cellEditorErrMsg = style(
      color("#a00")
    )

    private val autoCompleteDesc =
      styleS(color("#444"), fontStyle.italic, overflow.hidden, maxWidth(36 ex))

    val reqAutoComplete = styleC {
      val r = styleS(fontWeight.bold)
      r.named('req) :*: autoCompleteDesc.named('desc)
    }

    val codeRefToReqAutoComplete = styleC {
      val code  = styleS(fontWeight.bold)
      val pubid = styleS(paddingLeft(1 ex), color("#333"))
      code.named('code) :*: pubid.named('pubid) :*: autoCompleteDesc.named('desc)
    }

    def codeRefToGroupAutoComplete = reqAutoComplete

    val textEditPreview = style(
      padding(h = 0.8.ex, v = 0.2.em),
      border(solid, 1 px, "#222".color),
      minHeight(2 em),
      backgroundColor("#efe")
    )

  } // reqtable

  // ===================================================================================================================
  object widgets {

    private val hasError = mixin(
      color("#c00"),
      hasErrorBackground
    )

    private val hoverShowsInfo = cursor.help

    private val dead = mixin(
      textDecoration := ^.lineThrough,
      hasError
    )

    val blankLine = style(display.block, height(1 em))

    val ul = style(paddingLeft(2.4 ex))

    val tag = style(
      addClassName("label label-default"),
      hoverShowsInfo,
      marginRight(1 ex))

    val issue = style(hasError)

    val issueDesc = style(
      padding.horizontal(0.7 ex))

    // TODO Has color conflict
    val reqRef = styleF(D.alive)(a => styleS(
      display.inlineBlock,
      color("#2363A1"),
      mixinIf(a :: Dead)(dead),
      hoverShowsInfo))

    val groupRef = styleF(D.alive)(a => styleS(
      reqRef(a),
      mixinIf(a :: Dead)(hasError)
    ))

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

    val reqCodeTreeIndent = style(reqCodeTreePre, color("#dadada".color))
    val reqCodeTreeCode = style(reqCodeTreePre)
    val reqCodeFlat = style(reqCodePre, display.block)
  }

  // ===================================================================================================================

  private def init(a: StyleA*) = () // TODO add to ScalaCSS as (force)init(Objects) or something
  init(
    reqtable.sortCriteriaEditor.conclusiveColumnName,
    reqtable.table,
    widgets.tag)
//  ConsoleIO(_.log(render[String])).unsafePerformIO()
//  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
