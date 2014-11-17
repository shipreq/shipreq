package shipreq.webapp.client.util.ui.tablespec2

import design._
import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import monocle.{SimpleOptional, SimplePrism, SimpleLens, Lenser}
import shipreq.webapp.base.validation._
import shipreq.webapp.client.util.ui.Util.textChangeRecv
import scalajs.js.UndefOr
import scala.util.Try
import scalaz._, Scalaz._
import scalaz.effect.IO

object impls {

  def nopCB[S] = ReactS.retT[IO, S, Unit](())

  type ECB[S] = ReactST[IO, S, Unit]

  def cancelOnEscape[S](cb: ECB[S]): ReactKeyboardEventH => ECB[S] =
    e => e.key match {
      case "Escape" => // TODO use KeyValue
        val t = e.target
        ReactS.retM[IO, S, Unit](e.preventDefaultIO >> e.stopPropagationIO) >> cb.addCallback(IO(t.blur()))
      case _ =>
        nopCB
    }

//  def textEditor[S](node: Tag, T: ComponentStateFocus[S]): Editor[String, ECB[S], Modifier] =
//    ei => {
//      val base = node(cls := ei.cssClass, value := ei.data)
//      ei.editable match {
//        case None =>
//          base(readonly := true)
//        case Some(cb) =>
//          base(
//            onchange  ~~> T._runState(textChangeRecv(cb.onChange)),
//            onkeydown ~~> T._runState(cancelOnEscape(cb.onCancel)),
//            onblur    ~~> T.runState(cb.onEditFinished))
//      }
//    }

  abstract class ECB2[A] {
    type S
    def f: ComponentStateFocus[S]
    def cb: ReactST[IO, S, A]

    final def run = f.runState(cb)
  }
  object ECB2 {
    def apply[_S, A](_f: => ComponentStateFocus[_S], _cb: => ReactST[IO, _S, A]): ECB2[A] =
      new ECB2[A] {
        override type S = _S
        override def f = _f
        override def cb = _cb
      }
  }

  def textEditor2(node: Tag): Editor[String, String, ECB2, Modifier] =
    Editor(ei => {
      val base = node(cls := ei.cssClass, value := ei.data)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          base(
            onchange  ~~> textChangeRecv(cb.onChange(_).run),
            onkeydown ~~> cb.onCancel.f._runState(cancelOnEscape(cb.onCancel.cb)),
            onblur    ~~> cb.onEditFinished.run)
      }
    })

  val textInputEditor = textEditor2(input)
  val textareaEditor = textEditor2(textarea)

  def renderWithError[A, B, C[_]](editor: Editor[A, B, C, Modifier])(err: String): Editor[A, B, C, Modifier] =
    Editor(ei => div(editor render ei, div(cls := "errorMsg", err)))

  def editorWithError[A, B, C[_]](editor: Editor[A, B, C, Modifier]): EditorE[Option[String], A, B, C, Modifier] =
    _.fold(editor)(renderWithError(editor))

  def editorV[E, A, B, C[_], V](f: A => E, e: EditorE[E, A, B, C, V]): Editor[A, B, C, V] =
    Editor(i => e(f(i.data)) render i)

  def validateAndDisplayError[A, B, C[_]](f: A => Option[String], e: Editor[A, B, C, Modifier]): Editor[A, B, C, Modifier] =
    Editor(i => editorV(f, editorWithError(e)) render i)

  @deprecated("Need external validation (S⇒VP)", "")
  def composeEditorValidator[I, C[_]](v: ValidatorPlus[I, _, _], e: Editor[I, I, C, Modifier]): Editor[I, I, C, Modifier] = {
    type E = Editor[I, I, C, Modifier]
    val e1: E = e.mapOutput(v.liveCorrect)
    val e2: E = validateAndDisplayError(i => v.correctAndValidate(i).swap.toOption.map(_.toText), e1)
    e2
  }

  // ===================================================================================================================
  // example

  case class Age(value: Int)
  case class Person(id: Long, name: String, age: Age)

  val nameV: ValidatorPlus[String, String, String] = ???

  val ageV =
    ValidatorPlus[String, Option[Int], Age](
      CorrectionPart[String, Option[Int]](s => Try(Option(s.toInt)).getOrElse(None))(_.fold("")(_.toString)),
      ValidationPart[Option[Int], Age](???),
      _.replaceAll("\\D", ""))

  val nameE = textInputEditor
  val ageE = textInputEditor

  val nameE2 = composeEditorValidator(nameV, nameE)
  val ageE2 = composeEditorValidator(ageV, ageE)

  /*
  Storage.
  Different kinds. for each T: {saved, unsaved} ?
  Composable.

  just lenses.

  trait SavedStorage[S, Id, Data] {
    def idl(id: Id): SimpleLens[S, UndefOr[Data]]

    def contramap[X](f: SimpleLens[X, S]): SavedStorage[X, Id, Data] = {
      val x: SimpleLens[X, UndefOr[Data]] = f composeLens idl(???)
      ???
    }
  }

  trait SavedStorageChoose[S, Id, Data] {
  }
  */


  /*

  These are the editors for each field.
  These are the validators for each field.
  These fields make a row.

  Row type depends on underlying data type.


  case class RowState(n: String, a: String)
  private[this] def l = Lenser[RowState]
  val rowState_n = l(_.n)
  val rowState_a = l(_.a)

  val dirty: ECB2[RowState] = new ECB2[RowState] {
    type S = RowState
    val f: ComponentStateFocus[S] = ???
    val cb: ST = ReactS.getsT(s => IO(s))
  }
  implicit val dirtyBind: Bind[ECB2] = ???

  val rowE: Editor[RowState, RowState, ECB2, (Modifier, Modifier)] =
    editors2i(nameE, ageE, rowState_n, rowState_a, dirty)

    Row = lens + validator =
    */

  trait Row {
    type Ctx
    type S // State|Store
    final type C[A] = ReactST[IO, S, A]
    type Clean
    type Dirty
    type Validated

    def clean: Ctx => Clean
    def dirty: C[Dirty] // Read all stores in row, to build X

    def validate: (Ctx, Dirty) => Option[Validated] // Verify X

    def saveRequired: (Clean, Validated) => Boolean // check store for last clean and abort if NOP

    def save: Validated => C[Unit]
    def lock: C[Unit]

    def onChange(ctx: Ctx): C[Unit] =
      // TODO doesn't update dirty
      dirty.flatMap(d =>
        validate(ctx, d) match {
          case Some(v) if saveRequired(clean(ctx), v) =>
            save(v) flatMap (_ => lock)
          case _ =>
            nopCB[S]
        }
      )

    def getCSF: Ctx => ComponentStateFocus[S]

    def render[A,B,V](ctx: Ctx, e: Editor[A,B,ECB2,V]): V = {
      val cbs = EditorCallbacks[B, ECB2](
        b => ECB2(getCSF(ctx), onChange(ctx)),
        ???,
        ???)
      ???
    }
  }

}