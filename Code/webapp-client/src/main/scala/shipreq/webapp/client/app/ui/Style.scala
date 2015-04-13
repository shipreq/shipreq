package shipreq.webapp.client.app
package ui

import japgolly.scalacss.Defaults._
import japgolly.scalacss.ScalaCssReact._
import japgolly.scalacss.StyleS
import shipreq.webapp.base.text.Grammar
import shipreq.webapp.base.data.{Alive, Dead}
import shipreq.webapp.client.lib.ConsoleIO
import scalaz.syntax.equal._

object Style extends StyleSheet.Inline {
  import dsl._

  object Missing {
    import japgolly.scalacss._
    import DslBase.ToStyle

    def styleIf(b: Boolean)(t: ToStyle*)(implicit c: Compose): StyleS =
      if (b) styleS(t: _*)(c) else StyleS.empty
  }

  import Missing._

  import japgolly.scalacss.Color

//  private implicit class boolext(val b: Boolean) extends AnyVal {
//    import japgolly.scalacss._
//    import DslBase.ToStyle
//
//    def styleIf(t: ToStyle*)(implicit c: Compose): StyleS =
//      if (b) styleS(t: _*) else StyleS.empty
//
//    @inline def styleIfNot(t: ToStyle*)(implicit c: Compose): StyleS =
//      (!b).styleIf(t: _*)
//  }

   // ==================================================================================================================

  val dragHnd = style(
    color("#000"))

  val aliveDomain = Domain.ofValues[Alive](Alive, Dead)
  private def aliveStyle(f: Alive => StyleS): Alive => StyleA =
    styleF(aliveDomain)(f)

  object reqtable {
    import ui.reqtable.Column

    object sortingSettings {

      val row = boolStyle(on => styleS(
//        styleIf(!on)(
//          backgroundColor("#e2e2e2")),
        marginBottom(0.7 ex),
        paddingRight(1 ex)))

      def dragHnd = Style.dragHnd

      val dirSelect = style(
        width(28 ex))

      val field = boolStyle(on => styleS(
        marginLeft(1 ex),
        styleIf(!on)(
          //textDecoration := ^.lineThrough,
          color("#999"))))

      // ↑ inconclusive | conclusive ↓

      val conclusiveDir = style(
        marginLeft(4 ex))

      val conclusiveField = style(
        marginLeft(1 ex))
    }

    object columnSettings { // TODO This is crazy...
      import OrderedSubsetEditor.Styles

      val row = style()

      val prop = (on: Boolean) => Styles(row, dragHnd = sortingSettings.dragHnd, label = sortingSettings.field(on))
    }

    // http://stackoverflow.com/questions/446624/table-cell-widths-fixing-width-wrapping-truncating-long-words
    val table = style(
      marginTop(1.6 ex),
      width(100 %%))

    val mnemonicLen = Grammar.reqTypeMnemonic.length.total.max

    val columnPubid   = style(maxWidth((mnemonicLen + 5).ex))
    val columnReqType = style(maxWidth(mnemonicLen.ex))
//    val columnPubid   = style(maxWidth.fitContent)
//    val columnReqType = style(maxWidth.fitContent)

    val `N/A` = style(
      margin.horizontal(auto)
    )

    val cell = boolStyle(focus => styleS(
      border(1 px, solid, if (focus) Color("#006") else Color("#000")),
      padding(v = 2.px, h = 4.px),
      styleIf(focus)(
        backgroundColor("#e9e9ff"),
        outline(rgba(0, 0, 140, 0.15), 2 px, solid),
        outlineOffset(-1 px)
      )
    ))

    val cellEditor = boolStyle(hasError => styleS(
//      borderRadius(4 px),
      width(100 %%),
//      boxShadow := "inset 0 1px 1px rgba(0,0,0,.075)",
//      transition := "border-color ease-in-out .15s, box-shadow ease-in-out .15s",
      //border(1 px, solid, if (hasError) Color("#a94442") else Color("#666")),
//      outlineColor(if (hasError) Color("#a94442") else Color("#666")),
      styleIf(hasError)(hasErrorBackground, &.focus(outlineColor("#f88"))),
      padding.horizontal(0.8 ex)
    ))
//    val cellEditorO = boolStyle(hasError => styleS(
//      addClassNames("form-group", if (hasError) "has-error" else "has-success")
//    ))
//    val cellEditorI = boolStyle(hasError => styleS(
//      addClassNames("form-control")
//    ))

    val cellEditorErrMsg = style(
      color("#a00")
    )

    val reqAutoComplete = styleC {
      val r = styleS(fontWeight.bold)
      val d = styleS(color("#444"), fontStyle.italic, overflow.hidden, maxWidth(36 ex))
      r.named('req) :*: d.named('desc)
    }

    val textEditPreview = style(
      padding(h = 0.8.ex, v = 0.2.em),
      border(solid, 1 px, Color("#222")),
      backgroundColor("#efe")
    )

  } // reqtable


  val hasErrorBackground =
    backgroundColor("#fee")

  object widgets {

    val hasError = styleS(
      color("#c00"),
      hasErrorBackground
    )

    val hoverShowsInfo = cursor.help

    val dead = styleS(
      textDecoration := ^.lineThrough,
      hasError
    )

    val blankLine = style(display.block, height(1 em))

    val tag = style(
      addClassName("label label-default"),
      hoverShowsInfo,
      marginRight(1 ex))

    val issue = style(hasError)

    val issueDesc = style(
      padding.horizontal(0.7 ex))

    // TODO Has color conflict
    val reqRef = aliveStyle(a => styleS(
      display.inlineBlock,
      color("#2363A1"),
      styleIf(a ≟ Dead)(dead),
      hoverShowsInfo))

    val math = style(margin.horizontal(0.8 ex))
  }

  def damnit(a: StyleA*) = ()
  damnit(
    reqtable.sortingSettings.conclusiveDir,
    reqtable.columnSettings.row,
    reqtable.table,
    widgets.tag)
//  ConsoleIO(_.log(render[String])).unsafePerformIO()
  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
