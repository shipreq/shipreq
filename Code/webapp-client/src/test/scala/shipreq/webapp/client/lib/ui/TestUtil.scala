package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import monocle.macros.Lenser
import scalaz.Equal
import scalaz.std.AllInstances._
import scalaz.syntax.equal._
import scalaz.effect.IO
import shipreq.prop.test.Gen
import shipreq.base.util.Debug._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.TextMod._
import shipreq.webapp.base.validation.Constraints._
import shipreq.webapp.base.validation._
import shipreq.webapp.client.lib.{FailureIO, SuccessIO}
import Editors._
import RowStatus._

object TestUtil {

  def assertEq[A: Equal](actual: A, expect: A): Unit =
    assertEq(None, actual, expect)

  def assertEq[A: Equal](name: String, actual: A, expect: A): Unit =
    assertEq(name.some, actual, expect)

  def assertEq[A: Equal](name: Option[String], actual: A, expect: A): Unit =
    if (actual ≠ expect) {
      println()
      name.foreach(n => println(s">>>>>>> $n"))
      println(s"actual: [$actual]\nexpect: [$expect]")
      println()
      assert(false)
    }

  def assertRowStatusFailed(r: RowStatus): RowStatus.Failed =
    r match {
      case f@ RowStatus.Failed(_) => f
      case f => sys.error(s"Expected a failed row. Got: $f")
    }

  case class AB[A,B](a: A, b: B)

  def genAB[A, B](ga: Gen[A], gb: Gen[B]): Gen[AB[A,B]] =
    Gen.apply2(AB.apply[A, B])(ga, gb)

  type TestFields2[A, B] = FieldSet2[AB[A, B], A, B]

  def fields2[A,B](empty: (A,B)): TestFields2[A, B] =
    FieldSet2[AB[A,B]](_.a, _.b)(empty)

  implicit def eqCallbackEvent[B] = Equal.equalA[CallbackEvent[B]]

  implicit val eqRowStatus = Equal.equalA[RowStatus]

  val failedRowStatus =
    Failed(IO(()))

  def genRowStatus: Gen[RowStatus] =
    Gen.oneof(Sync, Locked, failedRowStatus)

  object SampleData_Person {

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

    val uniqueUsername = Uniqueness.entity[Person].applyO(_.id.some, _.username).fieldName(usernameF)

    val usernameV = usernameVU.liftS[VS].addValidation(uniqueUsername)

    val descVU = GenericValidators.optionalLargeText("Desc")
    val descV = descVU.liftS[VS]

    val personV = usernameV ⊗ descV

    val fields = FieldSet2[Person](_.username.value, _.desc getOrElse "")(("", "TODO"))

    val savedRowStore = SavedRowStore.fields(fields).keyedBy[Long]
    val newRowStore   = NewRowStore.of(fields)

    val needSave = SaveNeed.cmpToExtract((p: Person) => (p.username, p.desc))

    val person7 = Person(7, Username("mike"), None)
    val person4 = Person(4, Username("bob"), Some("Hello"))

    val sampleData = List(person4, person7)

    // TODO Use TypicalStoresAndState
    case class NewAndSavedRowState(newRow: newRowStore.State, savedRows: savedRowStore.State)
    object NewAndSavedRowState {
      private[this] def l = Lenser[NewAndSavedRowState]
      val _newRow      = l(_.newRow)
      val _savedRows   = l(_.savedRows)
      val savedRowStoreS = savedRowStore.contramap(_savedRows)
      val newRowStoreS   = newRowStore  .contramap(_newRow)

      val initialState = NewAndSavedRowState(newRowStore.initState, savedRowStore.initStateS(sampleData, _.id))

      case class SaveI(p: Option[Person], u: (Username, Option[String]), s: SuccessIO, f: FailureIO)
      type SaveIO = SaveI => IO[Unit]

      case class Props(fieldValidation: Boolean, updateRevert: Boolean, saveIO: Option[SaveIO]) {
        if (saveIO.isDefined && !updateRevert) sys.error("saveIO needs updateRevert")
      }

      class Backend(c: BackendScope[Props, NewAndSavedRowState]) {
        val e = {
          var e1 = textInputEditor.addCssClass("username")
          var e2 = textareaEditor.addCssClass("desc")
          if (c.props.fieldValidation) {
            e1 = e1.applyValidatorU(usernameVU)
            e2 = e2.applyValidatorU(descVU)
          }

          var en = Editor.merge2(fields, e1, e2).tupleI.strengthR[Option[Long]].zoomU[NewAndSavedRowState]

          if (c.props.updateRevert)
            en = en.applyRowUpdateAndRevert(savedRowStoreS, newRowStoreS)(_._2)

          c.props.saveIO.foreach(save => {
            val f = Persistence.asyncSaveS(personV, savedRowStoreS)(newRowStoreS,
              s => (savedRowStoreS.getAllP(s), None),
              k => s => (savedRowStoreS.getAllP(s), k.some),
              needSave,
              (u, s, f) => save(SaveI(None, u, s, f)),
              (p, u, s, f) => save(SaveI(p.some, u, s, f)),
              c runState _
            )
            en = en.applyOnEditFinishedK(f)(_._2)
          })
          en
        }

        def renderRow(a: e.InputA) =
          e.render(EditorI(a, "", e.editable(c runState _.st)))

        def render: ReactElement = {
          val newRow = newRowStoreS.getI(c.state).fold(EmptyTag)(i => {
            val v = renderRow((i, None))
            <.div(^.cls := "new", v._1, v._2)
          })
          val saved = savedRowStoreS.getAll(c.state).map(row => {
            val id = row.p.id
            val v = renderRow((row.i, id.some))
            <.div(^.key := id, ^.cls := s"id-$id", v._1, v._2)
          })
          <.section(newRow, saved.toReactNodeArray)
        }
      }

      val Component = ReactComponentB[Props]("NewAndSavedRowState")
        .initialState(NewAndSavedRowState.initialState)
        .backend(new Backend(_))
        .render(_.backend.render)
        .build
    }
  }
}
