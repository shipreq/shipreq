import monocle._
import monocle.syntax._
import monocle.function.Field1._
import monocle.function.Field2._
import org.scalajs.dom
import org.scalajs.dom.console
import scala.scalajs.js
import scalaz.{State, StateT, Scalaz, Bind}
import scalaz.syntax.bind._
import scalaz.std.option.optionInstance
import Scalaz.Id
import scalaz.effect.IO
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import japgolly.scalajs.react.ScalazReact._
import FormStuff._
import Lib._

object Phase2 extends js.JSApp {
  override def main(): Unit = {
    import Phase2.IssueConfig._

    IssueTypeTable(List(
      1L -> UserDefIssueType("TODO", None)
      ,2L -> UserDefIssueType("TBD", Some("To Be Decided."))
    )) render dom.document.getElementById("target")

    DragAndDrop.Component(List(
      DragAndDrop.Item(10, "Ten")
      ,DragAndDrop.Item(20, "Two Zero")
      ,DragAndDrop.Item(30, "Firty")
      ,DragAndDrop.Item(40, "Thorty")
      ,DragAndDrop.Item(50, "Fipty")
    )) render dom.document.getElementById("target2")
  }


  object IssueConfig {

    type UserDefIssueTypeId = Long
    type S = FormState
    type E = SPEC.E
    type P = UserDefIssueType
    type G = UserDefIssueType
    type Px = (UserDefIssueTypeId, P)
    type Unsaved = Option[E]
    type SaveMap = Map[UserDefIssueTypeId, (P, E)]

    case class UserDefIssueType(key: String, desc: Option[String])
    val keyL = SimpleLens2[UserDefIssueType](_.key)((a, b) => a.copy(key = b))
    val descL = SimpleLens2[UserDefIssueType](_.desc)((a, b) => a.copy(desc = b))

    val SPEC = Spec2(
      SpecSplice(keyL.get _, KeyValidator).edit(TextInputEditor),
      SpecSplice(descL.get _, DescValidator).edit(TextareaEditor),
      (UserDefIssueType.apply _).tupled)

    case class FormState(saved: SaveMap, unsaved: Unsaved)
    val savedL = SimpleLens2[FormState](_.saved)((a,b) => a.copy(saved = b))
    val unsavedL = SimpleLens2[FormState](_.unsaved)((a,b) => a.copy(unsaved = b))

    def mkPE(p: P) = (p, SPEC initial p)

    def storeUpdate(px: Px): S => S =
      savedL.modifyF(_ + (px._1 -> mkPE(px._2)))

    def fakeSave(p: Option[Px], g: UserDefIssueType) = IO[Px] {
      console.log(s"SAVING $p ⇒ $g")
      val newId = p.fold[UserDefIssueTypeId](666L)(_._1)
      (newId, g)
    }

    type RowId = Option[UserDefIssueTypeId]
    val SPECX = Spec2X(SPEC, Some(keyUniqueness), None)
    //def keyUniqueness = uniquenessRefl[S, String](_.saved.toStream.map(_._2._1.key))
//    def keyUniqueness = uniquenessRefl[Stream[UserDefIssueType], String](_.map(_.key))
//    def S2X(s: S): X = (s, )
    def keyUniqueness = uniqueness[S, RowId, (UserDefIssueTypeId, (P, E)), String](
      _.saved.toStream,
      (a,w) => w.fold(false)(_ == a._1),
      (a,i) => i == a._2._1.key
    )

    // ===============================================================================================
    object NewRow {
      private def empty: SPEC.E = ("","")

      def createS = State.modify[FormState](unsavedL.modifyF(_ orElse Some(empty)))

      private def storeInsert(px: Px): S => S =
        storeUpdate(px) compose unsavedL.setF(None)

      private val renderAttr = {
        val s2op: S => Option[P] = _ => None
        def setE(s: S, e: E): Option[S] = unsavedL.get(s).map(_ => unsavedL.set(s, Some(e)))
        val se = WierdLens[Option, S, S, E](unsavedL.get, setE)
        val saveIO: (S, G) => IO[S] = (s,g) => fakeSave(None, g).map(storeInsert(_)(s))
        SPECX.renderM(se, saveIO, s2op, None) _
      }

      private val delS = State.modify[S](_.copy(unsaved = None))

      private def renderRow(T: ComponentScope_SS[S], vv: SPEC.VV) = {
        val (key, desc) = vv
        //val ctrls = raw(S.unsaved.toString)
        val delButton = button(onclick ~~> T.runStateIO(NewRow.delS))("Cancel")
        tr(keyAttr := "new")(td(key), td(desc), td(delButton))
      }

      val row = new FullRow[Option, S, SPEC.VV, Tag, Unit](
          _ => renderAttr, (T,_,vv) => renderRow(T, vv))
    }

    // ===============================================================================================
    object SavedRow {
      private def rowL(id: UserDefIssueTypeId) = savedL composeLens SimpleLens2[SaveMap](_(id))((a,b) => a + (id -> b))

      private def renderAttr(id: UserDefIssueTypeId) = {
        val l: SimpleLens[S, (P, E)] = rowL(id)
        val sp: SimpleLens[S, P] = l |-> _1
        val se: SimpleLens[S, E] = l |-> _2
        val saverr = SavingThingy[S, G, Px, Px, Px](
          s => (id, sp get s),
          (px,g) => if (px._2 == g) None else Some(px),
          (px,g) => fakeSave(Some(px), g),
          storeUpdate)
        SPECX.render(se, saverr.save, sp.get, Some(id)) _
      }

      private def fakeDelete(id: UserDefIssueTypeId) = IO {
        console.log(s"DELETING $id")
      }

      private def delS(id: UserDefIssueTypeId) =
        runStoreU(fakeDelete(id), (s:S) => s.copy(saved = s.saved - id))

      private def renderRow(T: ComponentScope_SS[S], id: UserDefIssueTypeId, vv: SPEC.VV) = {
        val (key, desc) = vv
        val delButton = button(onclick ~~> T.runStateIO(SavedRow delS id))("Delete")
        //val ctrls = raw(s"${s.key} | ${s.desc}")
        tr(keyAttr := id)(td(key), td(desc), td(delButton))
      }

      val row = new FullRow[Id, S, SPEC.VV, Tag, UserDefIssueTypeId](
        renderAttr, renderRow)
    }

    // ===============================================================================================
    val IssueTypeTable = ReactComponentB[List[(UserDefIssueTypeId, UserDefIssueType)]]("IssueTypeTable")
      .getInitialState(p => FormState(p.map(x => x._1 -> mkPE(x._2)).toMap, None))
      .render(T => {
        val S = T.state
        //console.log(s"State = $S")

        def newRow = NewRow.row.render(T)(())
        def row = SavedRow.row.render(T)

        val rows = S.saved.toList.sortBy(_._2._1.key)

        // TODO handle empty table
        div(
          button(onclick ~~> T.runStateIO(NewRow.createS))("Create"),
          table(tbody(
            tr(th("Name"), th("Description"), th("Ctrls"))
            , newRow
            , rows.map(x => row(x._1)).toJsArray
          ))
        )
      }).create
    }

  // ===============================================================================================
  // ===============================================================================================
  // ===============================================================================================

//  trait SyntheticMouseEvent[N <: dom.Node] extends dom.MouseEvent with SyntheticEvent[N]
//  trait SyntheticDragEvent[N <: dom.Node] extends dom.DragEvent with SyntheticEvent[N]


  object DragAndDrop {

    case class Item(id: Int, name: String)

    val placeholder = dom.document.createElement("li")
    placeholder.className = "placeholder"

    class DNDB(T: BackendScope[_, List[Item]]) {
      type E = dom.HTMLElement
      type SE = SyntheticEvent[E] with dom.DragEvent

      def eh(f: SE => Unit) = (e: SE) => IO(f(e))

      var dragged: E = null
      var draggedStyle: String = null
      var over: E = null
      var nodeAfter: Boolean = _

      def dragStart: SyntheticEvent[E] => IO[Unit] = e => IO{
        val ee = e.dragEvent.get
        val t = e.currentTarget
        console.log("START: ", t, t.asInstanceOf[js.Dynamic].dataset.id)
        this.dragged = t
        this.draggedStyle = t.style.display
        e.asInstanceOf[js.Dynamic].dataTransfer.effectAllowed = "move"

        // Firefox requires calling dataTransfer.setData
        // for the drag to properly work
        //ee.dataTransfer.setData("text/html", t.toString)
      }

      def getDataId(e: E) =
        e.asInstanceOf[js.Dynamic].dataset.id.asInstanceOf[String].toInt

      def dragEnd: SyntheticEvent[E] => IO[Unit] = e => IO {
        //        console.log("END: ", dragged, over)
        if (this.dragged != null) {
          this.dragged.style.display = this.draggedStyle
          this.dragged.parentNode.removeChild(placeholder)

          // Update state
          val from = getDataId(dragged)
          val to = getDataId(over)
          console.log("END: ", from, to)
          T.modState(s => {
            val f = s.find(_.id == from).get
            s.flatMap(i => {
              var x = if (i.id == from) Nil else (i :: Nil)
              if (to == i.id) x = if (this.nodeAfter) x :+ f else f :: x
              x
            })
          })

          this.dragged = null
          this.over = null
        }
      }

      def dragOver: SyntheticEvent[E] => IO[Unit] = e => IO {
        if (this.dragged != null) {
          val ee = e.dragEvent.get
          e.preventDefault()
          this.dragged.style.display = "none"
          if (e.target.className != "placeholder") {
            this.over = e.target

            // Inside the dragOver method
            val relY = ee.clientY - this.over.offsetTop
            val height = this.over.offsetHeight / 2
            val parent = e.target.parentNode

            if (relY > height) {
              this.nodeAfter = true
              parent.insertBefore(placeholder, e.target.asInstanceOf[js.Dynamic].nextElementSibling.asInstanceOf[dom.Node])
            } else if (relY < height) {
              this.nodeAfter = false
              parent.insertBefore(placeholder, e.target)
            }
          }
        }
      }
    }

    val Component = ReactComponentB[List[Item]]("DragAndDrop")
      .getInitialState(p => p)
      .backend(new DNDB(_))
      .render(T => {
        console.log(s"State = ${T.state}")

        def renderItem(i: Item) =
          li(
            key := i.id
            , "data-id".attr := i.id
            ,"draggable".attr := "true"
            ,"onDragStart".attr ~~> T.backend.dragStart
            ,"onDragEnd".attr ~~> T.backend.dragEnd
          )(s"${i.name} / ${i.id}")

      div(
          h1("Drag and Drop"),
          ol(
            "onDragOver".attr ~~> T.backend.dragOver
          )(T.state map renderItem)
        )

      }).create

  }
}