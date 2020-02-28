package shipreq.webapp.client.project.test

import japgolly.scalajs.react._, vdom.html_<^._, ScalazReact._
import monocle.macros.Lenses
import scalaz.Equal
import scalaz.std.AllInstances._
import scalaz.syntax.equal._

import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.validation._, Simple._, Implicits._
import shipreq.webapp.base.data.TCB
import shipreq.webapp.client.project.app.pages.config_old.shared._
import Editors._
import Uniqueness.Util._

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

  val usernameV: Composite.Stateful[VS, String, String, Username] =
  CommonValidation.endoValidator.lengthInRange(2 to 16)
    .addInvalidator(CommonValidation.invalidator.whitelistCharRangeRegex("a-z0-9_")(Invalidity("can only contain letters, numbers and underscores.")))
    .addInvalidator(CommonValidation.invalidator.startsWithRegex("[a-z]")(Invalidity("must start with a letter.")))
    .addInvalidator(CommonValidation.invalidator.endsWithRegex("[a-z0-9]")(Invalidity("must end with a letter or a number.")))
    .prependCorrector(TextMod.noWhitespace.andThen(TextMod.lowerCase).correctFull.appendLive(_.toLowerCase))
    .toValidator
    .mapValid(Username.apply)
    .named(usernameF)
    .stateful((v, vs) => v.appendInvalidator(Uniqueness.within(excludeOptionalKey(vs._2, vs._1)(_.id).map(_.username))))

  val descV = CommonValidation.optionalLargeText.named("Desc").lift[VS]

  val personV = (s: VS) => usernameV(s).named tuple descV(s).named

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
        implicit def autoRunIDontCare[A](cb: CallbackTo[A]): A = cb.runNow()

        var e1 = textInputEditor.addCssClass("username")
        var e2 = textareaEditor.addCssClass("desc")
        if ($.props.fieldValidation) {
          e1 = e1.applyValidator(usernameV.stateless.unnamed)
          e2 = e2.applyValidator(descV.stateless.unnamed)
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

      //def renderRow(a: ((String, String), Option[Long])) =
      def renderRow(a: e.InputA) =
        e.render(EditorI(a, "", e.editable($ runState _.st)))

      def render(s: NewAndSavedRowState): VdomElement = {
        val newRow = newRowStoreS.getI(s).fold(EmptyVdom)(i => {
          val v = renderRow((i, None))
          <.div(^.cls := "new", v._1, v._2)
        })
        val saved = savedRowStoreS.getAll(s).map(row => {
          val id = row.p.id
          val v = renderRow((row.i, id.some))
          <.div(^.key := id, ^.cls := s"id-$id", v._1, v._2)
        })
        <.section(newRow, saved.toVdomArray)
      }
    }

    val Component = ScalaComponent.builder[Props]("NewAndSavedRowState")
      .initialState(NewAndSavedRowState.initialState)
      .renderBackend[Backend]
      .build
  }
}