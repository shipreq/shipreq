package shipreq.webapp.client.app.ui

import japgolly.scalacss.Defaults._
import japgolly.scalacss.ScalaCssReact._
import japgolly.scalacss.StyleS
import shipreq.webapp.client.lib.ConsoleIO

object Style extends StyleSheet.Inline {
  import dsl._

  object Missing {
    import japgolly.scalacss._
    import DslBase.ToStyle

    def styleIf(b: Boolean)(t: ToStyle*)(implicit c: Compose): StyleS =
      if (b) styleS(t: _*)(c) else StyleS.empty
  }

  import Missing._

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

  object reqtable {

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
          textDecoration := ^.lineThrough,
          color("#aaa"))))

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
  } // reqtable

  reqtable.sortingSettings
  reqtable.columnSettings
  ConsoleIO(_.info(s"Styles: ${Style.register.styles.length}")).unsafePerformIO()
}
