import scala.scalajs.js
import js.Object
import org.scalajs.dom
import japgolly.scalajs.react._
import vdom.ReactVDom._
import all._
import utest._
import utily.EditorStuff._
import utily.FormStuff._
import utily.SpecN._
import utily.Lib._
import scalaz.std.string.stringInstance
import scalaz.Scalaz.Id
import scalaz.effect.IO
import ScalazReact._
import japgolly.scalajs.react.test._

object Test extends TestSuite {

  lazy val A = ReactComponentB[Unit]("A").render((_,c) => p(cls := "AA", c)).createU
  lazy val B = ReactComponentB[Unit]("B").render(_ => p(cls := "BB", "hehehe")).createU
  lazy val rab = ReactTestUtils.renderIntoDocument(A(B()))

  val tests = TestSuite {

    case class Data(key: String, desc: Option[String]) //, num: Int)

//    case class Person(key: String, desc: Option[String], age: Int)
//    'a {
//      val nameL = WierdLens[Id, Person, Person, String](_.name, (a,b) => a.copy(name = b))
//      def saveIO(p: Person) = IO{println("CALLED!"); p.copy(age = 100)}
//
//      val f = new FormAttrShit[Person, String, String, String, Id](_ => KeyValidator, _.name, nameL, saveIO)
//      val p2: Person = ReactS.unlift(f.editEnd).exec(Person("hehe", 7)).unsafePerformIO()
//      println(p2)
//    }

    'b {
      def fakeSave(prev: Option[(Int, Data)], data: Data) = IO[(Int, Data)] {
        (9, data)
      }

      val PreSpec = SpecBuilder[Data](
        SpecAttr[Data](_.key)(KeyValidator)(TextInputEditor),
        SpecAttr[Data](_.desc)(DescValidator)(TextareaEditor)
      ).buildO(Data.apply).rowId[Int]
      val Spec = PreSpec.ctxAwareValidators(Some(PreSpec.uniquenessCheck(_.key)), None)
        .saveFn(fakeSave)
      val Comp = ReactComponentB[Map[Int, Data]]("Comp")
        .getInitialState(p => Spec.initialState(p))
        .render(T => {
        val S = T.state
        //console.log(s"State = $S")

        val savedRow = Spec.savedRow((T, id, vv) => {
          val (key, desc) = vv
          div(keyAttr := id)(div(ref := "hey")(key), div(desc))
        })
        val savedRows = Spec.renderSaved(T, savedRow)(_.sortBy(_._2.key))

        div(savedRows)
      }).create


      val data = Map(2 -> Data("ABC", None)) //, 3 -> Data("DEF", Some("YAG")))
      println(React renderComponentToStaticMarkup Comp(data))

      val c1 = Comp.apply(data)
      val c = ReactTestUtils.renderIntoDocument(c1)
      val r = c.asInstanceOf[js.Dynamic].refs
      println(s"REFS: $r")
      val r2 = c.asInstanceOf[js.Dynamic].refs.hey
      println(s" REF: $r2")
      val i = ReactTestUtils.findRenderedDOMComponentWithTag(c, "input")
      println(s"I = $i")
//      ReactTestUtils.Simulate.change(i, js.Dynamic.literal(data = "no"))
//      println(s"I = $i")
    }
  }
}
