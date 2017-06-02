package shipreq.webapp.client.project.app

import japgolly.scalajs.react.vdom.html_<^.{^ => ^^, _}
import japgolly.univeq._
import shipreq.webapp.client.base.CssSettings._
import scalacss.ScalaCssReact._
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

      val actionCtrlButton = style(
        marginRight(ctrlHGap).important)

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
        textAlign.right.important)

      val formCancelButton = style(
        (background := "#fff").important,
        borderColor(c"#27292a").important,
        marginRight(2 ex))

      val formCreateButton = style(
        (background := "#fff").important,
        color(c"#080").important,
        borderColor(c"#080").important,
        marginRight(`0`).important,
        fontWeight.bold.important)
    }

    object table {

      val table = style(
        margin(`0`).important)

      private val headerBase = style(
        padding(4.px).important)

      private val bodyBase = style(
        padding(4.px).important,
        verticalAlign.top.important)

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

      private val selectionColumnCell = styleS(
        width(24.px).important,
        textAlign.center.important)

      val selectionColumnHeader = style(selectionColumnCell, headerBase)
      val selectionColumnBody   = style(selectionColumnCell, bodyBase)

      val pubidColumnValue = styleF(D.live)(a => styleS(
        display.inline,
        whiteSpace.nowrap,
        mixinIf(a is Dead)(deadAndNotError)))

      val `N/A` = style(
        color(c"#666"),
        margin.horizontal(auto))

      val dataCell = styleF(D.`live * on`)(i => styleS(
        bodyBase,
        i match {
          case (Live, Off) => mixin()
          case (Dead, Off) => mixin(backgroundColor(c"#f5f5f5"))
          case (Live, On ) => mixin(backgroundColor(c"#ffc"))
          case (Dead, On ) => mixin(backgroundColor(c"#f5f5c4"))
        },
        &.focus(
          outline(solid, 2 px, c"#a333c8"))))

      val noContent = style(
        padding(2 em).important)
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
  }

  // ===================================================================================================================
  object deletionForm {

    val section = style(
      marginTop(2.3 em),
      marginBottom(1 em),
      fontWeight.bold)

    val row = styleF(D.live)(live => styleS(
      mixinIf(live is Dead)(backgroundColor(c"#fee"), color(c"#a00"))
    ))

    val indent: Int => TagMod =
      Memo(n => TagMod(^^.display.`inline-block`, ^^.width := s"${n * 3}ex"))

    val reqDesc =
      style(marginLeft(0.5 ex))

    val impliedByPrefix =
      style(marginRight(0.5 ex))

    val impliedByItem = styleF(D.live)(l => styleS(
      // hoverShowsInfo, // It's a link to ReqDetail now
      mixinIf(l is Live)(color(c"#111")),
      mixinIf(l is Dead)(
        //textDecoration := ^.lineThrough,
        color(c"#daa"))
    ))

    def subCodeCount = impliedByItem
  }

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
      mixinIf(live is Dead)(backgroundColor(rgba(0, 0, 0, .04)))))

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

      private val ctrlButton = style(
        unsafeChild("i")(fontSize(1.2 rem).important),
        margin(`0`).important,
        padding(0.3 rem).important,
        &.disabled(opacity(0.3).important))

      val ctrlButtonInsert     = style(ctrlButton, &.hover(color(c"#21BA45").important))
      val ctrlButtonDelete     = style(ctrlButton, &.hover(color(c"#DB2828").important))
      def ctrlButtonRestore    = ctrlButtonInsert
      def ctrlButtonShiftLeft  = ctrlButton
      def ctrlButtonShiftRight = ctrlButton
    }
  }

  // ===================================================================================================================
  object widgets {

    val richTextPreview = style(
      addClassNames("ui", "segments", "raised"))

    val richTextPreviewHeader = style(
      addClassNames("ui", "segment", "inverted"),
      paddingLeft(1 ex).important,
      paddingTop(0.3 em).important,
      paddingBottom(0.3 em).important,
      (background := c"#89d6e5").important,
      color(c"#0d1516").important)

    val richTextPreviewBody = style(
      padding(1 ex).important,
      addClassNames("ui", "segment"),
      (backgroundImage := "repeating-linear-gradient(-225deg,rgba(0,0,0,0),rgba(0,0,0,0)5ex,rgba(137,214,229,.1)5ex,rgba(137,214,229,.1)10ex)").important)

    private val refColour = color(c"#2363A1")

    val blankLine = style(display.block, height(1 em))

    val ul = style(paddingLeft(2.4 ex))

    private def tagBase(live: Live) = mixin(
      padding(4 px, 6 px).important,
      mixinIf(live is Dead)(&.not(_.hover)(textDecoration := ^.lineThrough)),
      hoverShowsInfo)

    private def tagLabelColour(live: Live) = live match {
      case Live => "blue"
      case Dead => "grey"
    }

    val tag = styleF(D.live)(live => styleS(
      tagBase(live),
      addClassName(s"ui label ${tagLabelColour(live)}")))

    val tagInText = styleF(D.`live * validity`){ case (l, v) => styleS(
      tagBase(l),
      mixinIf(l is Live)(refColour),
      mixinIf(l is Dead)(deadMaybeValid(v)))
    }

    val reqTypeShort = styleF(D.live)(a => styleS(
      hoverShowsInfo,
      mixinIf(a is Dead)(deadAndNotError)))

    val issue = style(hasError)

    val erroneousUseCaseStepRef = style(hasError)

    val issueDesc = style(
      padding.horizontal(0.7 ex))

    val pastPubid = style(deadMaybeValid(Valid))

    val reqRef = styleF(D.`live * validity`){ case (l, v) => styleS(
      // hoverShowsInfo, // It's a link to ReqDetail now
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

    object autoComplete {
      val itemTitle = style(
        fontWeight.bold)

      val itemTitle2 = style(
        paddingLeft(1 ex),
        color(c"#333"))

      val itemDesc = style(
        color(c"#444"),
        fontStyle.italic,
        overflow.hidden,
        maxWidth(36 ex))
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
    deletionForm.impliedByItem(Live),
    reqtable.creation.buttonDropdown,
    reqtable.filterEditor.input(Valid),
    reqtable.sortEditor.dragArea,
    reqtable.page.viewCtrls,
    reqtable.table.selectionColumnHeader,
    reqdetail.detailTable,
    reqdetail.useCaseStep.container,
    widgets.issue,
    widgets.autoComplete.itemTitle,
    widgets.reqTypeSelector.dropdown)
//  ConsoleIO(_.log(render[String])).unsafePerformIO()
//  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
