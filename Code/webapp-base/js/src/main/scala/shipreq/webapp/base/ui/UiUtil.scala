package shipreq.webapp.base.ui

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalaz.\/
import shipreq.webapp.base.lib.KeyHandler.Criterion
import shipreq.webapp.base.lib.KeyHandlers
import shipreq.webapp.base.validation.Simple

object UiUtil {

  def renderSimpleInvalidity(d: Simple.Invalidity \/ Any): Option[VdomTag] =
    d.fold(i => Some(renderSimpleInvalidity(i)), _ => None)

  def renderSimpleInvalidity(i: Simple.Invalidity): VdomTag = {
    def render1(err: String): VdomTag = <.span(err)
    if (i.tail.isEmpty)
      render1(i.head)
    else
      <.div(MutableArray(i.whole).sort.iterator.map(render1).intersperse(<.br).toTagMod)
  }

  def submitOnEnter(submit: Callback): KeyHandlers =
    Criterion.Enter.handle(submit) + Criterion.CtrlEnter.handle(submit)

}
