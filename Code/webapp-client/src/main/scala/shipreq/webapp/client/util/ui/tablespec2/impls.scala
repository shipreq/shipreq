package shipreq.webapp.client.util.ui.tablespec2

import design._
import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import monocle._
import monocle.function.{first, second}
import monocle.std.tuple2._
import shipreq.webapp.base.validation._
import shipreq.webapp.client.util.ui.Util.textChangeRecv
import scalajs.js.UndefOr
import scala.util.Try
import scalaz._, Scalaz._
import scalaz.effect.IO

object impls {

  def nopCB[S] = ReactS.retT[IO, S, Unit](())

  type RST[S, A] = ReactST[IO, S, A]

  type StrCf    = ComponentStateFocus[String]
  type StrCb[A] = RST[String, A]

  def cancelOnEscape[S](cb: RST[S, Unit]): ReactKeyboardEventH => RST[S, Unit] =
    e => e.key match {
      case "Escape" => // TODO use KeyValue
        val t = e.target
        ReactS.retM[IO, S, Unit](e.preventDefaultIO >> e.stopPropagationIO) >> cb.addCallback(IO(t.blur()))
      case _ =>
        nopCB
    }

  def textEditor(node: Tag): Editor[String, String, StrCb, StrCf, Modifier] =
    Editor[String, String, StrCb, StrCf, Modifier](ei => {
      val w = ei.world
      val base = node(cls := ei.cssClass, value := ei.data)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          base(
            onchange  ~~> w._runState(textChangeRecv(cb.onChange)),
            onkeydown ~~> w._runState(cancelOnEscape(cb.onCancel)),
            onblur    ~~> w.runState(cb.onEditFinished))
      }
    })

  val textInputEditor = textEditor(input)
  val textareaEditor  = textEditor(textarea)

  def renderWithError[A, B, C[_], W](editor: Editor[A, B, C, W, Modifier])(err: String): Editor[A, B, C, W, Modifier] =
    Editor(ei => div(editor render ei, div(cls := "errorMsg", err)))

  def editorWithError[A, B, C[_], W](editor: Editor[A, B, C, W, Modifier]): EditorE[Option[String], A, B, C, W, Modifier] =
    _.fold(editor)(renderWithError(editor))

  def editorV[E, A, B, C[_], W, V](f: A => E, e: EditorE[E, A, B, C, W, V]): Editor[A, B, C, W, V] =
    Editor(i => e(f(i.data)) render i)

  def validateAndDisplayError[A, B, C[_], W](f: A => Option[String], e: Editor[A, B, C, W, Modifier]): Editor[A, B, C, W, Modifier] =
    Editor(i => editorV(f, editorWithError(e)) render i)

  @deprecated("Need external validation (S⇒VP)", "")
  def composeEditorValidator[I, C[_], W](v: ValidatorPlus[I, _, _], e: Editor[I, I, C, W, Modifier]): Editor[I, I, C, W, Modifier] = {
    type E = Editor[I, I, C, W, Modifier]
    val e1: E = e.mapOutput(v.liveCorrect)
    val e2: E = validateAndDisplayError(i => v.correctAndValidate(i).swap.toOption.map(_.toText), e1)
    e2
  }

  class CsfWorldConv[S] extends WConv[ComponentStateFocus[S], S, ComponentStateFocus] {
    override def apply[A](l: SimpleLens[S, A]): ComponentStateFocus[S] => ComponentStateFocus[A] =
      _.focusState(l.get)(l.set)
  }

//  def editors2iq[I, I1,V1, I2,V2](e1: Editor[I1,I1,({type L[A] = ReactST[IO, I1, A]})#L, ComponentStateFocus[I1],V1],
//                                  e2: Editor[I2,I2,({type L[A] = ReactST[IO, I2, A]})#L, ComponentStateFocus[I2],V2],
//                                  f1: SimpleLens[I, I1], f2: SimpleLens[I, I2])
//  : Editor[I, I, ({type L[A] = ReactST[IO, I, A]})#L, ComponentStateFocus[I], (V1, V2)] =
//    editors2i[I, ({type L[A] = ReactST[IO, I, A]})#L, ComponentStateFocus[I], ComponentStateFocus, I1,V1, I2,V2](
//      e1, e2,
//      f1, f2,
//      new CsfWorldConv[I],
//      ReactS.getT[IO, I])

//  def editors2i_csf[I,C[_]: Bind, I1,V1, I2,V2](e1: Editor[I1,I1,C,ComponentStateFocus[I1],V1], e2: Editor[I2,I2,C,ComponentStateFocus[I2],V2],
//                                                  f1: SimpleLens[I, I1], f2: SimpleLens[I, I2],
//                                                  dirty: C[I]) : Editor[I, I, C, ComponentStateFocus[(I1,I2)], (V1, V2)] =


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

  @inline final def modStateL[M[_], S, T, A](l: SimpleLens[T, S], r: ReactST[M, S, A])(implicit M: Functor[M]): ReactST[M, T, A] =
    modState(l.get, l.set, r)

  @inline final def modState[M[_], S, T, A](f: T => S, g: (T, S) => T, r: ReactST[M, S, A])(implicit M: Functor[M]): ReactST[M, T, A] =
    StateT[M, StateAndCallbacks[T], A](tc => {
      val sc = StateAndCallbacks(f(tc.s), tc.cb)
      val x: M[(StateAndCallbacks[S], A)] = r(sc)
      val y: M[(StateAndCallbacks[T], A)] = M.map(x){
        case (z, a) => (StateAndCallbacks[T](g(tc.s, z.s), z.cb), a)
      }
      //      r. M.map(f(sc.s))(s2 => (sc withState s2,()) )
      y
    })

  type I = (String,String)
  type ICb[A] = RST[I, A]
//  def icb_scb(l: SimpleLens[I, String]): ICb ~> StrCb = new (ICb ~> StrCb) {
//    override def apply[A](fa: ICb[A]): StrCb[A] =
//      modStateL[IO, I, String, A](2, fa)
//  }

//  val e2 = editors2i[I,ICb,ComponentStateFocus[I],ComponentStateFocus, String,Modifier, String,Modifier](
//    nameE2.contramapC(icb_scb(_._1)),
//    ageE2.contramapC(icb_scb(_._2)),
//    first[I, String], second[I, String],
//    new CsfWorldConv[I],
//    ReactS.getT[IO, I]
//  )

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

//  trait Row {
//    type Ctx
//    type S // State|Store
//    final type C[A] = ReactST[IO, S, A]
//    type Clean
//    type Dirty
//    type Validated
//
//    def clean: Ctx => Clean
//    def dirty: C[Dirty] // Read all stores in row, to build X
//
//    def validate: (Ctx, Dirty) => Option[Validated] // Verify X
//
//    def saveRequired: (Clean, Validated) => Boolean // check store for last clean and abort if NOP
//
//    def save: Validated => C[Unit]
//    def lock: C[Unit]
//
//    def onChange(world: Ctx): C[Unit] =
//      // TODO doesn't update dirty
//      dirty.flatMap(d =>
//        validate(world, d) match {
//          case Some(v) if saveRequired(clean(world), v) =>
//            save(v) flatMap (_ => lock)
//          case _ =>
//            nopCB[S]
//        }
//      )
//
//    def getCSF: Ctx => ComponentStateFocus[S]
//
//    def render[A,B,V](world: Ctx, e: Editor[A,B,ECB2,V]): V = {
//      val cbs = EditorCallbacks[B, ECB2](
//        b => ECB2(getCSF(world), onChange(world)),
//        ???,
//        ???)
//      ???
//    }
//  }

}