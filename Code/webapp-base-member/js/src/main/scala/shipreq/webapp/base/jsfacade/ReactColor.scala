package shipreq.webapp.base.jsfacade

import japgolly.scalajs.react._
import scala.scalajs.js
import scala.scalajs.js.annotation._

object ReactColor {

  @js.native
  trait ColorObject extends js.Object {
    var hex: String
    var hsl: TinyColor.Hsl
    var hsv: TinyColor.Hsv
    var rgb: TinyColor.Rgb
  }

  object ColorObject {
    def apply(hex: String): ColorObject =
      apply(TinyColor(hex))

    def apply(c: TinyColor): ColorObject =
      js.Dynamic.literal(
        hex = c.toHex(),
        hsl = c.toHsl(),
        hsv = c.toHsv(),
        rgb = c.toRgb(),
      ).asInstanceOf[ColorObject]
  }

  // ===================================================================================================================
  // Github

  object Github {

    object Props {
      def apply(): Props =
        (new js.Object).asInstanceOf[Props]

      def hex(color   : String,
              onChange: String => Callback,
              colours : Colours,
              triangle: String = "hide",
             ): Props =
        js.Dynamic.literal(
          color    = color,
          onChange = Props.onChange(onChange),
          width    = colours.width,
          triangle = triangle,
          colors   = colours.array,
        ).asInstanceOf[Props]

      type OnChange = js.Function1[Change, Unit]

      def onChange(f: String => Callback): OnChange =
        (c: Change) => f(c.hex).runNow()
    }

    final case class Colours(colours: Seq[String], width: String) {
      private[Github] val array: js.Array[String] =
        js.Array(colours: _*)
    }

    object Colours {
      implicit def reusability: Reusability[Colours] =
        Reusability.byRef
    }

    @js.native
    trait Change extends js.Object {
      var hex: String
      var source: String
    }

    @js.native
    trait Props extends js.Object {
      var color: String

      var onChange: Props.OnChange

      /** Pixel value for picker width. Default "200px" */
      var width: String

      /** Either "hide", "top-left" or "top-right". Default "top-left" */
      var triangle: String

      /** Color squares to display. Default ['#B80000', '#DB3E00', '#FCCB00', '#008B02', '#006B76', '#1273DE', '#004DCF', '#5300EB', '#EB9694', '#FAD0C3', '#FEF3BD', '#C1E1C5', '#BEDADC', '#C4DEF6', '#BED3F3', '#D4C4FB'] */
      var colors: js.Array[String]

      // onSwatchHover - An event handler for onMouseOver on the <Swatch>s within this component. Gives the args (color, event)
    }

    @JSGlobal("GithubPicker.Github")
    @js.native
    object RawComponent extends js.Object

    val Component = JsComponent[Props, Children.None, Null](RawComponent)
  }

  // ===================================================================================================================
  // Chrome

  object Chrome {

    object Props {
      def apply(): Props =
        (new js.Object).asInstanceOf[Props]

      def apply(color       : TinyColor,
                onChange    : TinyColor => Callback,
                disableAlpha: Boolean,
               ): Props = {
        val p          = ColorObject(color).asInstanceOf[Props]
        p.onChange     = Props.onChange(o => onChange(TinyColor(o)))
        p.disableAlpha = disableAlpha
        p
      }

      def hex(color       : String,
              onChange    : String => Callback,
              disableAlpha: Boolean,
             ): Props =
        apply(TinyColor(color), (c: TinyColor) => onChange(c.toHex()), disableAlpha)

      def onChange(f: ColorObject => Callback): js.Function1[ColorObject, Unit] =
        f(_).runNow()
    }

    @js.native
    trait Props extends ColorObject {
      var onChange: js.Function1[ColorObject, Unit]

      /** Remove alpha slider and options from picker. Default false */
      var disableAlpha: Boolean

      // renderers - Object, Use { canvas: Canvas } with node canvas to do SSR
    }

    @JSGlobal("ChromePicker.Chrome")
    @js.native
    object RawComponent extends js.Object

    val Component = JsComponent[Props, Children.None, Null](RawComponent)
  }
}
