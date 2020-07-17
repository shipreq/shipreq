package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html

/** http://semantic-ui.com/elements/header.html */
object Header {

  def h4 = <.h4(^.cls := "ui header")

  sealed abstract class Attr(cls: ClassName) extends HasClass(cls)
  object Attr {
    case object Block    extends Attr("block")
    case object Dividing extends Attr("dividing")
    case object Inverted extends Attr("inverted")
    implicit def univEq: UnivEq[Attr] = UnivEq.derive
  }

  sealed abstract class Type(val tag: VdomTagOf[html.Heading])
  object Type {
    case object H1 extends Type(<.h1)
    case object H2 extends Type(<.h2)
    case object H3 extends Type(<.h3)
    case object H4 extends Type(<.h4)
    case object H5 extends Type(<.h5)
    case object H6 extends Type(<.h6)
    implicit def univEq: UnivEq[Type] = UnivEq.derive
  }

  sealed abstract class State(c: ClassName) extends HasClass(c)
  object State {
    case object Default  extends State(NoClass)
    case object Disabled extends State("disabled")
    implicit def univEq: UnivEq[State] = UnivEq.derive
  }

  case class Style(tipe  : Type,
                   attr  : Multiple[Attr] = Multiple.empty,
                   state : State          = State.Default,
                   colour: Colour         = Colour.Default,
                   size  : Size           = Size.Default,
                   other : TagMod         = EmptyVdom) {

    val tag = tipe.tag(^.cls := "ui header" <+ attr <+ state <+ colour <+ size)(other)
  }

  def apply(style: Style, content: TagMod): VdomTagOf[html.Heading] =
    style.tag(content)

  def apply(style: Style, icon: Icon, content: TagMod): VdomTagOf[html.Heading] =
    style.tag(
      icon.tag,
      <.div(^.cls := "content", content))

  def apply(style: Style, icon: Icon, content: TagMod, subHeader: TagMod): VdomTagOf[html.Heading] =
    style.tag(
      icon.tag,
      <.div(^.cls := "content", content,
        <.div(^.cls := "sub header", subHeader)))
}
