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

  @deprecated("no","")
  def composeEditorValidator[I, C, D](v: Validator[I, _, _], e: Editor[I, I, C, D, Modifier]): Editor[I, I, C, D, Modifier] =
    e.applyInputValidation(v)
      .applyLiveCorrection(v)
      .applyPostCorrection(v.cp)

  @deprecated("no","")
  def composeEditorValidatorS[S, I, C, D](v: ValidatorS[S, I, _, _], e: Editor[I, I, C, D, Modifier]): Editor[(S, I), I, C, D, Modifier] =
    e.applyInputValidationSL(v)
      .applyLiveCorrection(v)
      .applyPostCorrectionS(v.cp)(_._1)

  // ↑ Library ↑
  // ===================================================================================================================
  // ↓ Application ↓

  object Example {

    case class Age(value: Int)
    case class Person(id: Long, name: String, age: Age)

    val personFields = FieldSet2[Person](_.name, _.age.toString)

    val nameV: Validator[String, String, String] = ???
    val ageV: Validator[String, Option[Int], Age] = ???

    val nameE2 = composeEditorValidator(nameV, textInputEditor)
    val ageE2 = composeEditorValidator(ageV, textInputEditor)

    // This is what uniqueness validation of name would probably look like ↙
    type NameSW = (Map[Long, String], Long)
    type NameSWI = (NameSW, String)
    def nameUniqueness(names: Map[Long, String], id: Long, name: String): Option[VFailure] =
      (names - id).forall(_._2 != name) match {
        case true  => None
        case false => Some(uniquenessFailure("Name"))
      }
    def uniquenessFailure(fieldName: String): VFailure =
      VFailure.forField(fieldName, NonEmptyList("must be unique."))
    def tovps[S,A](f: (S,InputCorrected[A]) => Option[VFailure]): ValidationPartS[S, A, A] =
      new ValidationPartS((s,a) => f(s,a) match {
        case None    => Success(a.value)
        case Some(r) => Failure(r)
      })
    val nameUniqueVPS = tovps[NameSW, String]((a,b) => nameUniqueness(a._1, a._2, b))
    val nameV2 = nameV.liftS[NameSW].addValidation(nameUniqueVPS)
    val nameE3 = nameE2.applyInputValidationSL(nameV2)

    type CompositeC = (personFields.Field, RU)

    type SWII = (NameSWI, String)
    val mergedE = Editor.merge2(personFields, nameE3, ageE2).pairI

    object ManualExample1_split_editors {

      case class Props(ppl: Map[Long, Person])
      
      val savedStore = SavedRowStore.of(personFields).keyedBy[Long]
      case class ZeState(saved: savedStore.State)
      val savedStoreZ = savedStore.contramap(SimpleLens[ZeState](_.saved)((a,b) =>  a.copy(saved = b)))
      val ZS = ReactS.FixT[IO, ZeState]

      def updatex(id: Long, b: personFields.FieldValue) = ZS.modS(savedStoreZ.setField(id, b))
      def revertx(id: Long, f: personFields.Field)      = ZS.modS(savedStoreZ.revertField(id, f))
      def lockrow(id: Long)                             = ZS.modS(savedStoreZ.setStatus(id, RowStatus.Locked))

      def validaterow(ss: NameSW, id: Long, ok: ((String, Age)) => ReactST[IO, ZeState, Unit], ko: VFailure => ReactST[IO, ZeState, Unit]) =
        ZS.liftR{ s =>
          val i = s.saved(id).i
          personV.correctAndValidate(ss, i) match {
            case scalaz.Success(v) => ok(v)
            case scalaz.Failure(f) => ko(f)
          }
        }

      type CompositeC2 = (personFields.Field, ReactST[IO, ZeState, Unit])
      val mergedE2 = mergedE
        .strengthR[Long]
        .mapC(_ map2 (_.zoomU[ZeState]))
        .modCallbacksA(a => _.pmodC(c => {
          case OnChange(b) => c map2 (_ >> updatex(a._2, b))
          case OnCancel    => c map2 (_ >> revertx(a._2, c._1))
        }))

      val personV = nameV2 *** ageV.liftS[NameSW]
      val mergedE3 = mergedE2.modCallbacksA(a => {
        val (((namesw, _), _), id) = a
        _.pmodC(c => {
          case OnEditFinished(b) => c map2 (_ >> validaterow(namesw, id, v => lockrow(id), _ => ZS.ret(())))
        })
      })

      class TopBackend(c: BackendScope[Props, ZeState]) {

        val editable = {
          val cbRealise: (Any,CompositeC2) => IO[Unit] = (_,x) => c.runState(x._2)
          Some(EditorCallbacks[personFields.FieldValue, CompositeC2, IO[Unit]](cbRealise))
        }

        def tableProps = TableProps(rowpropsa(c.state.saved))

        def rowpropsa(saved: savedStore.State): Vector[SavedRowProps] = {
          val names = saved.mapValues(_.i._1)
          saved.foldLeft(Vector.empty[SavedRowProps])((q, a) => q :+ rowprops1(names, a._1, a._2))
        }

        def rowprops1(names: Map[Long, String], id: Long, s: savedStore.Row): SavedRowProps = {
          val nameswi: NameSWI = ((names, id), s.i._1)
          val swii: SWII = (nameswi, s.i._2)
          SavedRowProps(id, EditorInput((swii, id), "", editable))
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

      case class SavedRowProps(key: Long, ei: EditorInput[(SWII, Long), personFields.FieldValue, CompositeC2, IO[Unit]])
      val savedrow = ReactComponentB[SavedRowProps]("savedrow")
        .stateless
        .render((p, _) => {
        val (n, a) = mergedE2.render(p.ei)
        tr(key := p.key, n, a)
      })
      .build
    }
  }
}