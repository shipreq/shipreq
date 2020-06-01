package shipreq.webapp.client.project.widgets.dev

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{Element, html}
import shipreq.webapp.base.lib.DomUtil._

/** This component renders all chars, measures their widths, and prints the results to the console.
  *
  * The results should then be copied and pasted in to `ww/CharWidths.scala`
  */
object CharWidthsGen {

  final class Backend($: BackendScope[Unit, Unit]) {

    private val ref = Ref[html.Element]

    private val span = <.span(^.wordBreak.`keep-all`, ^.margin := "0", ^.padding := "0", ^.outline := "0")

    private val lenAttrName = "data-len"
    private val lenAttr = VdomAttr(lenAttrName)

    private val rangeLo: Vector[(Int, Int)] =
      (32 to 127).toVector.map(i => (i, i))

    private val rangeMid: Vector[(Int, Int)] =
      Vector((128, 255), (256,1023))

    private val rangeHi: Vector[(Int, Int)] =
      (1024 to 65535).by(1024).toVector.map(i => (i,i+1023))

    private val range: Vector[(Int, Int)] =
      rangeLo ++ rangeMid ++ rangeHi

    def render: VdomNode = {
      def asd(s: String) = span(s"|$s|", lenAttr := s.length.toString)
      val data = <.div(asd(""), range.map{case (x, y) => (x to y).map(_.toChar.toString).mkString}.toTagMod(asd)).withRef(ref)
      val button = <.button("print again", ^.onClick --> printWidths)
      <.div(button, data)
    }

    def printWidths: Callback =
      for (root <- ref.get) yield {

        def len(e: Element, control: Double): Double = {
          val l = (e.getBoundingClientRect().width - control).max(0)
          val x = e.getAttribute(lenAttrName).toInt
          if (x > 1) l / x else l
        }

        val control = len(root.children(0), 0)
        val lens = root.children.iterator.drop(1).map(len(_, control))
          .map("%.6f".format(_).replaceFirst("\\.?0+$", ""))
          .toVector

        var s = ""
        def println(t: String): Unit = s += t + "\n"
        lens.take(rangeLo.length).grouped(8).foreach(g => println(g.mkString(",") + ","))
        lens.drop(rangeLo.length).take(rangeMid.length).foreach(l => println(l + ","))
        for ((l, i) <- lens.drop(rangeLo.length + rangeMid.length).zipWithIndex) println(s"$l, // ${rangeHi(i)._1}")
        org.scalajs.dom.console.log(s)
      }
  }

  val Component = ScalaComponent.builder[Unit]
    .renderBackend[Backend]
    .componentDidMount(_.backend.printWidths)
    .shouldComponentUpdateConst(false)
    .build
}