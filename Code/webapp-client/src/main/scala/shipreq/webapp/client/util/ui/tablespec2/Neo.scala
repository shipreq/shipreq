package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import monocle._
import shipreq.webapp.base.validation._
import shipreq.webapp.client.util.ui.Util._
import scala.util.Try
import scalaz.effect.IO
import scalajs.js.undefined
import scalaz._, Scalaz._
import shipreq.base.util.ScalaExt._

object Neo {

  // Args
  // - for each arg, measure variance of each type
  // - variance will determine what is needed to shift type later (functor variance)

  // Functions
  // - in & out of each type determine subtype variance of class's type members

  // Experiences
  // - If a type τ doesn't have the desired functoral variance, split it into two types, accept fn τ₁→τ₂.
  // - Going too abstract too soon means you could develop something nice/moral that can't do what you need it to.

  // ===================================================================================================================

  case class EditorCallbacks[-B, -C, +D](onChange: (B, C) => D,
                                     onCancel: C => D,
                                     onEditFinished: (B, C) => D) {

    @inline private def mp[X,Y,Z](f: ((B, C) => D) => ((X, Y) => Z), g: (C => D) => (Y => Z)) =
      EditorCallbacks[X, Y, Z](f(onChange), g(onCancel), f(onEditFinished))

    def mapB[X](f: X => B): EditorCallbacks[X, C, D] = mp(h => (b,c) => h(f(b), c), identity)
    def mapC[X](f: X => C): EditorCallbacks[B, X, D] = mp(h => (b,c) => h(b, f(c)), f.andThen)
    def mapD[X](f: D => X): EditorCallbacks[B, C, X] = mp(h => (b,c) => f(h(b, c)), f.compose)

    def modB_onChange      [X <: B](f: X => X): EditorCallbacks[X, C, D] = copy(onChange       = (b,c) => onChange      (f(b), c))
    def modB_onEditFinished[X <: B](f: X => X): EditorCallbacks[X, C, D] = copy(onEditFinished = (b,c) => onEditFinished(f(b), c))
  }

  case class EditorInput[+A, -B, -C, +D](data: A,
                                     cssClass: String,
                                     editable: Option[EditorCallbacks[B, C, D]]) {

    def mapA[X](f: A => X): EditorInput[X, B, C, D] = copy(data = f(data))
    def mapB[X](f: X => B): EditorInput[A, X, C, D] = mapCB(_ mapB f)
    def mapC[X](f: X => C): EditorInput[A, B, X, D] = mapCB(_ mapC f)
    def mapD[X](f: D => X): EditorInput[A, B, C, X] = mapCB(_ mapD f)
//    def dimap[X, Y](f: A => X, g: Y => B): EditorInput[X, Y, C] =
//      copy(data = f(data), editable = editable.map(_ contramap g))

    @inline final def mapCB[X,Y,Z](f: EditorCallbacks[B,C,D] => EditorCallbacks[X,Y,Z]): EditorInput[A,X,Y,Z] =
      copy(editable = editable map f)

    def modB_onChange      [X <: B](f: X => X): EditorInput[A, X, C, D] = mapCB(_ modB_onChange f)
    def modB_onEditFinished[X <: B](f: X => X): EditorInput[A, X, C, D] = mapCB(_ modB_onEditFinished f)
  }

  case class Editor[-A, +B, +C, -D, +V](render: EditorInput[A, B, C, D] => V) {
    def mapA[X](f: X => A): Editor[X, B, C, D, V] = Editor(i => render(i mapA f))
    def mapB[X](f: B => X): Editor[A, X, C, D, V] = Editor(i => render(i mapB f))
    def mapC[X](f: C => X): Editor[A, B, X, D, V] = Editor(i => render(i mapC f))
    def mapD[X](f: X => D): Editor[A, B, C, X, V] = Editor(i => render(i mapD f))

//    def mapCB[X,Y,Z](f: EditorCallbacks[X,Y,Z] => EditorCallbacks[B,C,D]): Editor[A,X,Y,Z,V] = Editor(i => render(i mapCB f))
    def modB_onChange      [X >: B](f: X => X): Editor[A, X, C, D, V] = Editor(i => render(i modB_onChange f))
    def modB_onEditFinished[X >: B](f: X => X): Editor[A, X, C, D, V] = Editor(i => render(i modB_onEditFinished f))
  }

  type RU = ReactST[IO, Unit, Unit]
  val RU = ReactS.FixT[IO, Unit]
  val nopRU = RU.ret(())

  def textEditor(node: Tag): Editor[String, String, RU, IO[Unit], Modifier] =
    Editor(ei => {
      val base = node(cls := ei.cssClass, value := ei.data)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          base(
            onchange  ~~> textChangeRecv(cb.onChange(_, nopRU)),
            onblur    ~~> textChangeRecv(cb.onEditFinished(_, nopRU)),
            onkeydown ~~> cb.onCancel.compose(cancelOnEscape))
      }
    })

  def cancelOnEscape: ReactKeyboardEventH => RU =
    e => e.key match {
      case "Escape" => // TODO use KeyValue
        val t = e.target
        RU.callback(IO(t.blur()), e.preventDefaultIO >> e.stopPropagationIO)
      case _ =>
        nopRU
    }

  val textInputEditor = textEditor(input)
  val textareaEditor  = textEditor(textarea)

  type EditorE[E, A, B, C, D, V] = E => Editor[A, B, C, D, V]

  def renderWithError[A, B, C, D](editor: Editor[A, B, C, D, Modifier])(err: String): Editor[A, B, C, D, Modifier] =
    Editor(ei => div(editor render ei, div(cls := "errorMsg", err)))

  def editorWithError[A, B, C, D](editor: Editor[A, B, C, D, Modifier]): EditorE[Option[String], A, B, C, D, Modifier] =
    _.fold(editor)(renderWithError(editor))

  def editorV[E, A, B, C, D, V](f: A => E, e: EditorE[E, A, B, C, D, V]): Editor[A, B, C, D, V] =
    Editor(i => e(f(i.data)) render i)

  def validateAndDisplayError[A, B, C, D](f: A => Option[String], e: Editor[A, B, C, D, Modifier]): Editor[A, B, C, D, Modifier] =
    Editor(i => editorV(f, editorWithError(e)) render i)

  /*
  def validateAndDisplayError[A, B, C, D](f: A => Option[String]): Endo[Editor[A, B, C, D, Modifier]] =
    Endo(e => Editor(i => editorV(f, editorWithError(e)) render i))

  def applyLiveCorrection[A, B, C, D, V](v: ValidatorPlus[B, _, _]): Endo[Editor[A, B, C, D, V]] =
    Endo(_ modB_onChange v.liveCorrect)

  def applyPostCorrection[A, B, C, D, V, T](v: CorrectionPart[B, T]): Endo[Editor[A, B, C, D, V]] =
    Endo(_.modB_onEditFinished(b => v.ci(v.correct(b).value)))

  def applyInputValidation[A, B, C, D](v: Validator[A, _, _]): Endo[Editor[A, B, C, D, Modifier]] =
    validateAndDisplayError(i => v.correctAndValidate(i).swap.toOption.map(_.toText))

  def composeEditorValidator[I, C, D](v: ValidatorPlus[I, _, _]): Endo[Editor[I, I, C, D, Modifier]] =
    applyInputValidation[I, I, C, D](v) compose
      applyPostCorrection(v.cp) compose
        applyLiveCorrection(v)

  val nameE2 = composeEditorValidator(nameV).run(nameE)
  val ageE2 = composeEditorValidator(ageV).run(ageE)
  */

  /*
  def applyLiveCorrection[A, B, C, D, V](v: ValidatorPlus[B, _, _], e: Editor[A, B, C, D, V]): Editor[A, B, C, D, V] =
    e.modB_onChange(v.liveCorrect)

  def applyPostCorrection[A, B, C, D, V, T](v: CorrectionPart[B, T], e: Editor[A, B, C, D, V]): Editor[A, B, C, D, V] =
    e.modB_onEditFinished(b => v.ci(v.correct(b).value))

  def applyInputValidation[A, B, C, D](v: Validator[A, _, _], e: Editor[A, B, C, D, Modifier]): Editor[A, B, C, D, Modifier] =
    validateAndDisplayError(i => v.correctAndValidate(i).swap.toOption.map(_.toText), e)

  //  def uniqueness[S, A, B, C, D](f: (S,A), e: Editor[A, B, C, D, Modifier]): Editor[(S,A), B, C, D, Modifier] =
  //    validateAndDisplayError(i => v.correctAndValidate(i).swap.toOption.map(_.toText), e)

  def composeEditorValidator[I, C, D](v: ValidatorPlus[I, _, _], e: Editor[I, I, C, D, Modifier]): Editor[I, I, C, D, Modifier] =
    applyInputValidation(v,
      applyPostCorrection(v.cp,
        applyLiveCorrection(v,
          e)))
  */

  implicit final class EditorExt[A,B,C,D,V](val e: Editor[A,B,C,D,V]) extends AnyVal {
    type Self = Editor[A, B, C, D, V]
    def applyLiveCorrection(v: ValidatorPlus[B, _, _]): Self =
      e.modB_onChange(v.liveCorrect)

    def applyPostCorrection[T](v: CorrectionPart[B, T]): Self =
      e.modB_onEditFinished(b => v.ci(v.correct(b).value))
  }

  implicit final class EditorExtV[A,B,C,D](val e: Editor[A,B,C,D,Modifier]) extends AnyVal {
    type Self = Editor[A, B, C, D, Modifier]

    def applyInputValidation(v: Validator[A, _, _]): Self =
      validateAndDisplayError(i => v.correctAndValidate(i).swap.toOption.map(_.toText), e)
  }

  def composeEditorValidator[I, C, D](v: ValidatorPlus[I, _, _], e: Editor[I, I, C, D, Modifier]): Editor[I, I, C, D, Modifier] =
    e.applyInputValidation(v)
      .applyLiveCorrection(v)
      .applyPostCorrection(v.cp)

  // ===================================================================================================================

  object Example {

    @deprecated("????", "")
    def ???? = scala.Predef.???

    case class Age(value: Int)
    case class Person(id: Long, name: String, age: Age)

    val nameV: ValidatorPlus[String, String, String] = ???

    val ageV =
      ValidatorPlus[String, Option[Int], Age](
        CorrectionPart[String, Option[Int]](s => Try(Option(s.toInt)).getOrElse(None))(_.fold("")(_.toString)),
        ValidationPart[Option[Int], Age](???),
        _.replaceAll("\\D", ""))


    // ValidationPlus isn't helpful. LiveCorrect used in isolation from Validator
    val nameV_1: Validator[String, String, String] = nameV
    // TODO can't contramap Validator because I in invariant & needs xmap.

    val nameE = textInputEditor
    val ageE = textInputEditor

    val nameE2 = composeEditorValidator(nameV, nameE)
    val ageE2 = composeEditorValidator(ageV, ageE)

    // This is what uniqueness validation of name would probably look like ↙
    type NameSWI = (Map[Long, String], Long, String)
    val nameE3 = nameE2.mapA[NameSWI](_._3)

    object ManualExample1_split_editors {
      object RowStatus
      case class Props(ppl: Map[Long, Person])
      case class RowState(i: (String,String), rowStatus: RowStatus.type)
      type SavedState = Map[Long, RowState]
      case class ZeState(saved: SavedState)
      val ZS = ReactS.FixT[IO, ZeState]

      class TopBackend(c: BackendScope[Props, ZeState]) {

        def tableProps = TableProps(rowpropsa(c.state.saved))

        def update1(id: Long): (String, RU) => IO[Unit] = ????
        def update2(id: Long): (String, RU) => IO[Unit] =
          (i, ru) => c.runState(
            ru.zoomU[ZeState] >>
              ZS.modS{ s =>
                val nv = s.saved(id).i put2 i
                s.copy(saved = s.saved + (id -> RowState(nv, RowStatus)))
              }
          )

        def revert1(id: Long): RU => IO[Unit] = ????
        def revert2(id: Long): RU => IO[Unit] =
          ru => c.runState(
            ru.zoomU[ZeState] >>
              ZS.modS{ s =>
                // save as update except for this line here ↙
                val i = c.props.ppl(id).age.toString
                val nv = s.saved(id).i put2 i
                s.copy(saved = s.saved + (id -> RowState(nv, RowStatus)))
              }
            )

        def rowpropsa(saved: SavedState): Vector[SavedRowProps] = {
          val names = saved.mapValues(_.i._1)
          saved.foldLeft(Vector.empty[SavedRowProps])((q, a) => q :+ rowprops1(names, a._1, a._2))
        }

        def rowprops1(names: Map[Long, String], id: Long, s: RowState): SavedRowProps = {
          val nameswi: NameSWI = (names, id, s.i._1)
          SavedRowProps(id,
            EditorInput(nameswi, "", Some(EditorCallbacks[String, RU, IO[Unit]](update1(id), revert1(id), ???))),
            EditorInput(s.i._2, "", Some(EditorCallbacks[String, RU, IO[Unit]](update2(id), revert2(id), ???))))
        }
      }

      val outmost = ReactComponentB[Props]("Outmost")
        .getInitialState(p => ZeState(p.ppl.mapValues(v => RowState((v.name, v.age.toString), RowStatus))))
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

      case class SavedRowProps(key: Long, nameEI: EditorInput[NameSWI, String, RU, IO[Unit]], ageEI: EditorInput[String, String, RU, IO[Unit]])
      val savedrow = ReactComponentB[SavedRowProps]("savedrow")
        .stateless
        .render((p, _) => {
        //val (n, a) = e2.render(???)
        val n = nameE3 render p.nameEI
        val a = ageE2 render p.ageEI
        tr(key := p.key, n, a)
      })
        .build
    }
  }
  //  type S = Int
  //  type T = List[Int]
  //  val e1 = textEditor(input)
  //  val e2 = e1.mapC(_.zoomU[S])
  //
  //  val F: ComponentStateFocus[T] = ???
  //  val e3 = e2.mapC(_.zoom2[T](_.head, (a,b) => b :: a))
  //  type ST = ReactST[IO, T, Unit]
  //  val cbs = EditorCallbacks[String, ST, IO[Unit]](
  //    (i,st) => F.runState(st >> updateState(i)),
  //    st => F.runState(st),
  //    st => F.runState(st >> validateSaveLockRow))
  //
  //  def updateState(i: String): ST = ???
  //  def validateSaveLockRow: ST = ???
}