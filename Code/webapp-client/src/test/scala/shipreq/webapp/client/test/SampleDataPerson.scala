package shipreq.webapp.client.test

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import monocle.macros.Lenses
import scalaz.Equal
import scalaz.std.AllInstances._
import scalaz.syntax.equal._

import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.util.TextMod._
import shipreq.webapp.base.validation.Constraints._
import shipreq.webapp.base.validation._
import shipreq.webapp.client.lib.TCB
import shipreq.webapp.client.lib.ui._
import Editors._

object SampleDataPerson {

  case class Username(value: String)
  object Username {
    implicit val equal = Equal.equalA[Username]
  }
  case class Person(id: Long, username: Username, desc: Option[String])
  object Person {
    implicit val equal = Equal.equalA[Person]
  }

  type VS = (Stream[Person], Option[Long])

  val usernameF = "Username"

  val usernameVU = Validator(
    CorrectionPartU
      .endo(noWhitespace andThen lowerCase)
      .addLiveCorrect(_.toLowerCase),
    ValidationPartU.forConstraint(usernameF,
      lengthInRange(2 to 16)
        + whitelistCharsR("a-z0-9_")("can only contain letters, numbers and underscores.")
        + startsWithR("[a-z]")("must start with a letter.")
        + endsWithR("[a-z0-9]")("must end with a letter or a number.")
    )) map Username.apply

  val uniqueUsername = Uniqueness.entity[Person].optk(_.id.some).v(_.username).fieldName(usernameF)

  val usernameV = usernameVU.liftS[VS].addValidation(uniqueUsername)

  val descVU = GenericValidators.optionalLargeText("Desc")
  val descV = descVU.liftS[VS]

  val personV = usernameV ⊗ descV

  val fields = FieldSet2[Person](_.username.value, _.desc getOrElse "")(("", "TO"+"DO"))

  val savedRowStore = SavedRowStore.fields(fields).keyedBy[Long]
  val newRowStore   = NewRowStore.of(fields)

  val needSave = SaveNeed.cmpToExtract((p: Person) => (p.username, p.desc))

  val person7 = Person(7, Username("mike"), None)
  val person4 = Person(4, Username("bob"), Some("Hello"))

  val sampleData = List(person4, person7)

  // TODO Use TypicalStoresAndState
  @Lenses
  case class NewAndSavedRowState(newRow: newRowStore.State, savedRows: savedRowStore.State)
  object NewAndSavedRowState {
    val savedRowStoreS = savedRowStore.contramap(savedRows)
    val newRowStoreS   = newRowStore  .contramap(newRow)

    val initialState = NewAndSavedRowState(newRowStore.initState, savedRowStore.initStateS(sampleData, _.id))

    case class SaveI(p: Option[Person], u: (Username, Option[String]), s: TCB.Success, f: TCB.Failure)
    type SaveIO = SaveI => Callback

    case class Props(fieldValidation: Boolean, updateRevert: Boolean, saveIO: Option[SaveIO]) {
      if (saveIO.isDefined && !updateRevert) sys.error("saveIO needs updateRevert")
    }

    class Backend($: BackendScope[Props, NewAndSavedRowState]) {
      val e = {
        var e1 = textInputEditor.addCssClass("username")
        var e2 = textareaEditor.addCssClass("desc")
        if ($.props.fieldValidation) {
          e1 = e1.applyValidatorU(usernameVU)
          e2 = e2.applyValidatorU(descVU)
        }

        var en = Editor.merge2(fields, e1, e2).tupleI.strengthR[Option[Long]].zoomU[NewAndSavedRowState]

        if ($.props.updateRevert)
          en = en.applyRowUpdateAndRevert(savedRowStoreS, newRowStoreS)(_._2)

        $.props.saveIO.foreach(save => {
          val f = Persistence.asyncSaveS(personV, savedRowStoreS)(newRowStoreS,
            s => (savedRowStoreS.getAllP(s), None),
            k => s => (savedRowStoreS.getAllP(s), k.some),
            needSave,
            (u, s, f) => save(SaveI(None, u, s, f)),
            (p, u, s, f) => save(SaveI(p.some, u, s, f)),
            $ runState _
          )
          en = en.applyOnEditFinishedK(f)(_._2)
        })
        en
      }

      def renderRow(a: e.InputA) =
        e.render(EditorI(a, "", e.editable($ runState _.st)))

      def render: ReactElement = {
        val newRow = newRowStoreS.getI($.state).fold(EmptyTag)(i => {
          val v = renderRow((i, None))
          <.div(^.cls := "new", v._1, v._2)
        })
        val saved = savedRowStoreS.getAll($.state).map(row => {
          val id = row.p.id
          val v = renderRow((row.i, id.some))
          <.div(^.key := id, ^.cls := s"id-$id", v._1, v._2)
        })
        <.section(newRow, saved.toReactNodeArray)
      }
    }

    val Component = ReactComponentB[Props]("NewAndSavedRowState")
      .initialState(NewAndSavedRowState.initialState)
      .renderBackend[Backend]
      .build
  }
}