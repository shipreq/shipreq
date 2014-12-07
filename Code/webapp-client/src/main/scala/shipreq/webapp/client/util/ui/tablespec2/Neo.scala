package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.prefix_<*._, ScalazReact._
import monocle._
import monocle.syntax._
import shipreq.webapp.base.validation2._
import scala.util.Try
import scalaz.effect.IO
import scalajs.js.undefined
import scalaz._, Scalaz._
import shipreq.base.util.ScalaExt._
import scala.language.reflectiveCalls
import Editors._

object Neo {
  @deprecated("????", "")
  def ???? = scala.Predef.???

  // Args
  // - for each arg, measure variance of each type
  // - variance will determine what is needed to shift type later (functor variance)

  // Functions
  // - in & out of each type determine subtype variance of class's type members

  // Experiences
  // - If a type τ doesn't have the desired functoral variance, split it into two types, accept fn τ₁→τ₂.
  // - Going too abstract too soon means you could develop something nice/moral that can't do what you need it to.

  // ===================================================================================================================

  // ↑ Abstract ↑
  // ===================================================================================================================
  // ↓ Library ↓

  // ↑ Library ↑
  // ===================================================================================================================
  // ↓ Application ↓

  object Example {

    case class Age(value: Int)
    case class Person(id: Long, name: String, age: Age)

    val personFields = FieldSet2[Person](_.name, _.age.toString)(("", ""))

    val nameV: ValidatorU[String, String, String] = ???
    val ageV: ValidatorU[String, Option[Int], Age] = ???

    val nameUnique = Uniqueness.entity[Person](_.id, _.name).fieldName("Name")
    type NameSW = nameUnique._S
    type NameSWI = (NameSW, String)
    val nameV2 = nameV.liftS[NameSW].addValidation(nameUnique)
    val nameE = textInputEditor.applyValidator(nameV2)

    val ageE = textInputEditor.applyValidatorU(ageV)

    case class Props(ppl: Map[Long, Person])

    val savedStore = SavedRowStore.of(personFields).keyedBy[Long]
    case class ZeState(saved: savedStore.State)
    val savedStoreZ = savedStore.contramap(SimpleLens[ZeState](_.saved)((a,b) =>  a.copy(saved = b)))

    type Realiser = ReactST[IO, ZeState, Unit] => IO[Unit]
    val ZS = ReactS.FixT[IO, ZeState]

    def nameSW(s: ZeState): NameSW = ????

    type SWII = (NameSWI, String)
    val personV = nameV2 *** ageV.liftS[NameSW]

    val personE = Editor.merge2(personFields, nameE, ageE).tupleI
      .zoomU[ZeState]
      .cmapA[(SWII, Long, Realiser)](_._1)
      .modCallbacksA(a => {
        val (_, id, realiser) = a
        h => h.paddST {
          case OnChange(b)       => updateField(id, b)
          case OnCancel          => revertField(id, h.data)
          case OnEditFinished(_) => validateAndSave(id, realiser)
        }
      })

    def updateField(id: Long, b: personFields.FieldValue) = ZS.modT(savedStoreZ.setField(id, b))
    def revertField(id: Long, f: personFields.Field)      = ZS.modT(savedStoreZ.revertField(id, f))

    def validateAndSave(id: Long, realise: Realiser) = {
      import NeoSaves._
      type S = ZeState
      type I = personV._I
      type U = personV._V
      type P = Person
      validateAndSaveAsync[S, personV._S, P, U, I](
        personV, //      validator: Validator[S, I, _, U],
        nameSW, //      st: S => T,
        savedStoreZ.getI(id), //      si: S => I,
        savedStoreZ.getP(id), //      sp: S => P,
        savedStoreZ.setStatusST[IO](id), //      setStatus: SetRowStatus[S]
        ???, //      needSave: (U, P) => SaveNeed,
        ???, //      asyncSaveIO: (P, U, Any, Any) => IO[Unit],
        realise) //      realise: ReactST[IO, S, Unit] => IO[Unit],
    }

    class TopBackend(c: BackendScope[Props, ZeState]) {

      val realiser: Realiser = c.runState(_)

      val editable = personE.editable(c runState _.st)

      def tableProps = TableProps(rowpropsa(c.state.saved))

      def rowpropsa(saved: savedStore.State): Vector[SavedRowProps] = {
        val ppl = saved.values.toStream.map(_.p)
        saved.foldLeft(Vector.empty[SavedRowProps])((q, a) => q :+ rowprops1(ppl, a._1, a._2))
      }

      def rowprops1(ppl: Stream[Person], id: Long, s: savedStore.Row): SavedRowProps = {
        val nameswi: NameSWI = ((ppl, id), s.i._1)
        val swii: SWII = (nameswi, s.i._2)
        SavedRowProps(id, EditorI((swii, id, realiser), "", editable))
      }
    }

    val outmost = ReactComponentB[Props]("Outmost")
      .getInitialState(p => ZeState(savedStore initStateM p.ppl))
      .backend(new TopBackend(_))
      .render((p, s, b) =>
        <.div(<.h1("Hi!"), tablec(b.tableProps))
      )
      .build

    case class TableProps(saved: Vector[SavedRowProps])
    val tablec = ReactComponentB[TableProps]("table")
      .stateless
      .render((p,_) =>
        <.table(
          <.thead("Name", "Age"),
          <.tbody(p.saved.map(savedrow(_)).toReactNodeArray))
      )
      .build

    case class SavedRowProps(key: Long, ei: personE.Input)
    val savedrow = ReactComponentB[SavedRowProps]("savedrow")
      .stateless
      .render((p, _) => {
        val (n, a) = personE.render(p.ei)
        <.tr(*.key := p.key, n, a)
    })
    .build
  }
}