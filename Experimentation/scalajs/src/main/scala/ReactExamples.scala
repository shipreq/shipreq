import org.scalajs.dom.console
import scalaz.syntax.bind._
import scalaz.effect.IO
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import japgolly.scalajs.react.ScalazReact._
import shipreq.webapp.client.ui.DND

object ReactExamples {

  object DragAndDrop {

    case class Item(id: Int, name: String)
    implicit val itemEq = scalaz.Equal.equalRef[Item]

    val RowComp = DND.Child.dndItemComponent[Item](
      (i, hnd) => hnd :: raw(s"${i.id} | ${i.name}") :: Nil)

    case class ParentState(items: List[Item], dnd: DND.Parent.PState[Item], i: Int)

    val Component = ReactComponentB[List[Item]]("DragAndDrop")
      .initialStateP(p => ParentState(p, DND.Parent.initialState, 0))
      .render(T => {
      console.log(s"DND.State = ${T.state}")
      val itemsState = T.focusState(_.items)((a, b) => a.copy(items = b))
      val dndState = T.focusState(_.dnd)((a, b) => a.copy(dnd = b))

      def move(from: Item, to: Item) =
        IO{ console.log(s"...Before = ${T.state}") } >>
        IO{ itemsState.modState(DND.move(from, to)) } >>
        IO{ console.log(s"....After = ${T.state}") }

      def renderItem(i: Item) =
        li(key := i.id)(RowComp((i, DND.Parent.cProps(dndState, i, move ))))

      div(
        h1("Drag and Drop"),
        ol(T.state.items.map(renderItem).toJsArray)
      )
    }).create

    def demo =
      DragAndDrop.Component(List(
        DragAndDrop.Item(10, "Ten")
        ,DragAndDrop.Item(20, "Two Zero")
        ,DragAndDrop.Item(30, "Firty")
        ,DragAndDrop.Item(40, "Thorty")
        ,DragAndDrop.Item(50, "Fipty")
      ))
  }
}
