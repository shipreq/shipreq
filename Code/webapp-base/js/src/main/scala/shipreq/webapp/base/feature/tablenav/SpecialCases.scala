package shipreq.webapp.base.feature.tablenav

import japgolly.scalajs.react._
import japgolly.scalajs.react.internal.Box
import org.scalajs.dom.html
import scala.scalajs.js

object SpecialCases {

  private final val attrName = "_sqTNx"

  final case class Ctx(table: html.Table, event: ReactKeyboardEventFromHtml) {

    val target =
      event.currentTarget

    lazy val tbody =
      table.children.iterator.filter(_.tagName.toUpperCase == "TBODY").next()

    def bodyRows =
      tbody.children.length

    def bodyRow(row: Int) = {
      val idx = if (row >= 0) row else bodyRows + row
      tbody.children(idx)
    }
  }

  type Handler = Ctx => CallbackOption[Unit]

  private type RawHandler = js.UndefOr[Box[Handler]]

  def apply(table: html.Table)(f: Handler): Callback =
    Callback {
      val h: RawHandler = Box(f)
      table.asInstanceOf[js.Dynamic].updateDynamic(attrName)(h)
    }

  private[tablenav] def run(table: html.Table, e: ReactKeyboardEventFromHtml): CallbackOption[Unit] = {
    val raw = table.asInstanceOf[js.Dynamic].selectDynamic(attrName).asInstanceOf[RawHandler]
    raw.orNull match {
      case null    => CallbackOption.fail[Unit]
      case handler => handler.unbox(Ctx(table, e))
    }
  }
}
