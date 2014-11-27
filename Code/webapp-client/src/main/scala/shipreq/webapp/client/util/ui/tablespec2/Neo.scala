package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
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

  def composeEditorValidatorU[I, C, D](v: ValidatorU[I, _, _], e: Editor[I, I, C, D, Modifier]): Editor[I, I, C, D, Modifier] =
    e.applyInputValidation(v)
      .applyLiveCorrection(v)
      .applyPostCorrection(v.cp)

  def composeEditorValidator[S, I, C, D](v: Validator[S, I, _, _], e: Editor[I, I, C, D, Modifier]): Editor[(S, I), I, C, D, Modifier] =
    e.applyInputValidationSL(v)
      .applyLiveCorrection(v)
      .applyPostCorrectionS(v.cp)(_._1)

  def validateS[S, I, O](v: Validator[S, I, _, O]): I => ReactS[S, ValidationResult[O]] =
    i => ReactS.gets(s => v.correctAndValidate(s, i))



  // ↑ Library ↑
  // ===================================================================================================================
  // ↓ Application ↓

  object Example {

    case class Age(value: Int)
    case class Person(id: Long, name: String, age: Age)

    val personFields = FieldSet2[Person](_.name, _.age.toString)

    val nameV: ValidatorU[String, String, String] = ???
    val ageV: ValidatorU[String, Option[Int], Age] = ???

    val nameUnique = Uniqueness.entity[Person](_.id, _.name).fieldName("Name")
    type NameSW = nameUnique._S
    type NameSWI = (NameSW, String)
    val nameV2 = nameV.liftS[NameSW].addValidation(nameUnique)
    val nameE = composeEditorValidator(nameV2, textInputEditor)

    val ageE = composeEditorValidatorU(ageV, textInputEditor)

    case class Props(ppl: Map[Long, Person])

    val savedStore = SavedRowStore.of(personFields).keyedBy[Long]
    type Realiser = ReactST[IO, ZeState, Unit] => IO[Unit]
    case class ZeState(saved: savedStore.State)
    val savedStoreZ = savedStore.contramap(SimpleLens[ZeState](_.saved)((a,b) =>  a.copy(saved = b)))
    val ZS = ReactS.FixT[IO, ZeState]
    val nop = ZS.ret(()) // TODO add nop and & _nop to React
    val _nop = (_: Any) => nop

    def nameSW(s: ZeState): NameSW = ????

    type SWII = (NameSWI, String)
    val personV = nameV2 *** ageV.liftS[NameSW]

    def validateAndSave(id: Long, realise: ReactST[IO, ZeState, Unit] => IO[Unit]) = {
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
        ???, //      needSave: (U, P) => SaveNeed,
        ???, //      asyncSaveIO: (P, U, Any, Any) => IO[Unit],
        realise, //      realise: ReactST[IO, S, Unit] => IO[Unit],
        savedStoreZ.setStatusS(id) //      setStatus: SetRowStatus[S]
      )
    }

    val personE = Editor.merge2(personFields, nameE, ageE).pairI
      .cmapA[(SWII, Long, Realiser)](_._1)
      .mapC(_ map2 (_.zoomU[ZeState]))
      .modCallbacksA(a => {
        val (_, id, realiser) = a
        _.pmodC(c => {
          case OnChange(b)       => c map2 (_ >> updatex(id, b))
          case OnCancel          => c map2 (_ >> revertx(id, c._1))
          case OnEditFinished(_) => c map2 (_ >> validateAndSave(id, realiser))
        })
      })

    type CompositeC = (personFields.Field, ReactST[IO, ZeState, Unit])

    def updatex(id: Long, b: personFields.FieldValue) = ZS.modS(savedStoreZ.setField(id, b))
    def revertx(id: Long, f: personFields.Field)      = ZS.modS(savedStoreZ.revertField(id, f))
    def lockrow(id: Long)                             = ZS.modS(savedStoreZ.setStatus(id, RowStatus.Locked))

    class TopBackend(c: BackendScope[Props, ZeState]) {

      val realiser: Realiser = c.runState(_)

      val editable = {
        val cbRealise: (Any,CompositeC) => IO[Unit] = (_,x) => c.runState(x._2)
        Some(EditorCallbacks[personFields.FieldValue, CompositeC, IO[Unit]](cbRealise))
      }

      def tableProps = TableProps(rowpropsa(c.state.saved))

      def rowpropsa(saved: savedStore.State): Vector[SavedRowProps] = {
        val ppl = saved.values.toStream.map(_.p)
        saved.foldLeft(Vector.empty[SavedRowProps])((q, a) => q :+ rowprops1(ppl, a._1, a._2))
      }

      def rowprops1(ppl: Stream[Person], id: Long, s: savedStore.Row): SavedRowProps = {
        val nameswi: NameSWI = ((ppl, id), s.i._1)
        val swii: SWII = (nameswi, s.i._2)
        SavedRowProps(id, EditorInput((swii, id, realiser), "", editable))
      }
    }

    val outmost = ReactComponentB[Props]("Outmost")
      .getInitialState(p => ZeState(savedStore initStateM p.ppl))
      .backend(new TopBackend(_))
      .render((p, s, b) =>
      div(h1("Hi!"), tablec(b.tableProps))
      )
      .build

    case class TableProps(saved: Vector[SavedRowProps])
    val tablec = ReactComponentB[TableProps]("table")
      .stateless
      .render((p,_) =>
      table(
        thead("Name", "Age"),
        tbody(p.saved.map(savedrow(_)).asJsArray))
      )
      .build

    case class SavedRowProps(key: Long, ei: EditorInput[(SWII, Long, Realiser), personFields.FieldValue, CompositeC, IO[Unit]])
    val savedrow = ReactComponentB[SavedRowProps]("savedrow")
      .stateless
      .render((p, _) => {
      val (n, a) = personE.render(p.ei)
      tr(key := p.key, n, a)
    })
    .build
  }
}