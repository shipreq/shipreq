package shipreq.webapp.client.base.ui.semantic

/*
import japgolly.scalajs.react.FunctionalComponent
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.univeq.UnivEq

/** http://semantic-ui.com/views/card.html */
case class Card(tag: ReactTag)

object Card {

  case class Style(colour: Colour = Colour.Default,
                   other : TagMod = EmptyTag) {
    val tag = divCls("ui card" <+ colour)(other)
  }

  def apply(style: Style, icon: Icon, header: TagMod, desc: TagMod): Card =
    Card(
      style.tag(
        <.div(^.cls := "content pic", icon.tag),
        <.div(^.cls := "content",
          <.div(^.cls := "header", header),
          <.div(^.cls := "description", desc))))

}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

/** http://semantic-ui.com/views/card.html#cards */
object Cards {

  sealed abstract class Columns(cls: ClassName) extends HasClass(cls)
  object Columns {
    case object Default extends Columns(NoClass)
    case object Two     extends Columns("two")
    case object Three   extends Columns("three")
    case object Four    extends Columns("four")
    case object Five    extends Columns("five")
    case object Six     extends Columns("six")
    case object Seven   extends Columns("seven")
    case object Eight   extends Columns("eight")
    case object Nine    extends Columns("nine")
    implicit def univEq: UnivEq[Columns] = UnivEq.derive
  }

  case class Style(columns: Columns = Columns.Default) {
    val tag = divCls("ui cards" <+ columns)
  }

  final case class Props(style: Style, cards: Seq[Card]) {
    @inline def render = Component(this)
  }

  private def render(p: Props) =
    p.style.tag(p.cards.map(_.tag): _*)

  val Component = FunctionalComponent(render)
}
*/
