package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.Colour

/** This renders a table of colours to compare black & white foregrounds.
  * It was used to test that [[Colour.foreground]] was choosing the desired colours.
  *
  * In fact, when it was confirmed that it wasn't, this was used to derive a desirable formula and confirm it.
  */
object ColourTest {
  import Ordering.Double.TotalOrdering

  def demo = {

    val chars = "fc840".toList

    val colours =
      for {
        r <- chars
        g <- chars
        b <- chars
      } yield Colour.force(s"#$r$g$b")

    def cmp(bg: Colour, fg: Colour) =
      TagMod(
        ^.backgroundColor := bg.value,
        ^.color := fg.value,
        ^.textAlign.right,
        "%.2f".format(bg.contrastRatio(fg)),
      )

    val td = <.td(^.border := "solid 1px #888", ^.padding := "0 2ex", ^.lineHeight := "1.2em")


    <.table(^.borderCollapse.collapse,
      <.tbody(
        ^.fontSize := "125%",
        colours.sortBy(_.luminanace).toTagMod { c =>
          <.tr(
            td(<.pre(^.margin := "0", c.value)),
            td(cmp(c, Colour.white)),
            td(cmp(c, Colour.black)),
            td("%.2f".format(c.luminanace)),
            td(cmp(c, c.foreground)),
          )
        }
      )
    )
  }

}
