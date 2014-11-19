package shipreq.webapp.client.util.ui.tablespec2

import tryagain._
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
import shipreq.base.util.ScalaExt._

object impls2 {

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

  abstract class ECB2 {
    type S
    type A = Unit
    def f: ComponentStateFocus[S]
    def cb: ReactST[IO, S, A]

    final def run: IO[A] = f.runState(cb)
    final def proc[R](g: (ComponentStateFocus[S], ReactST[IO, S, A]) => R): R = g(f, cb)
  }
  object ECB2 {
    def apply[_S](_f: => ComponentStateFocus[_S], _cb: => ReactST[IO, _S, Unit]): ECB2 =
      new ECB2 {
        override type S = _S
        override def f = _f
        override def cb = _cb
      }

    def nop[_S](_f: => ComponentStateFocus[_S]): ECB2 =
      new ECB2 {
        override type S = _S
        override def f = _f
        override def cb = ReactS.retM[IO, S, Unit](IO(()))
      }
  }

  def textEditor(node: Tag): Editor[String, String, ECB2, Modifier] =
    Editor(ei => {
      val base = node(cls := ei.cssClass, value := ei.data)
      ei.editable match {
        case None =>
          base(readonly := true)
        case Some(cb) =>
          base(
            onchange  ~~> textChangeRecv(cb.onChange(_).run),
            onkeydown ~~> cb.onCancel.proc((f, cb) => f._runState(cancelOnEscape(cb))),
            onblur    ~~> cb.onEditFinished.run)
      }
    })

  val textInputEditor = textEditor(input)
  val textareaEditor  = textEditor(textarea)

  def renderWithError[A, B, C](editor: Editor[A, B, C, Modifier])(err: String): Editor[A, B, C, Modifier] =
    Editor(ei => div(editor render ei, div(cls := "errorMsg", err)))

  def editorWithError[A, B, C](editor: Editor[A, B, C, Modifier]): EditorE[Option[String], A, B, C, Modifier] =
    _.fold(editor)(renderWithError(editor))

  def editorV[E, A, B, C, V](f: A => E, e: EditorE[E, A, B, C, V]): Editor[A, B, C, V] =
    Editor(i => e(f(i.data)) render i)

  def validateAndDisplayError[A, B, C](f: A => Option[String], e: Editor[A, B, C, Modifier]): Editor[A, B, C, Modifier] =
    Editor(i => editorV(f, editorWithError(e)) render i)

  @deprecated("Need external validation (S⇒VP)", "")
  def composeEditorValidator[I, C](v: ValidatorPlus[I, _, _], e: Editor[I, I, C, Modifier]): Editor[I, I, C, Modifier] = {
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

  val bothV = nameV *** ageV

  val nameE = textInputEditor
  val ageE = textInputEditor

  val nameE2 = composeEditorValidator(nameV, nameE)
  val ageE2 = composeEditorValidator(ageV, ageE)

  val e2: Editor[(String, String), String \/ String, (ECB2, ECB2), (Modifier, Modifier)] =
    nameE2 compose ageE2

  type EditorState = (String, String)
  val ideal_e: (() => EditorState) => Editor[EditorState, EditorState, ECB2, (Modifier, Modifier)] =
    getes => Editor(i => {
      val es = getes()
      val i1 = i.dimap[String, String](_._1, j => es put1 j)
      val i2 = i.dimap[String, String](_._2, j => es put2 j)
      (nameE2 render i1, ageE2 render i2)
    })

  object ManualExample1_split_editors {
    object RowStatus
    case class Props(ppl: Map[Long, Person])
    case class RowState(i: (String,String), rowStatus: RowStatus.type)
    type SavedState = Map[Long, RowState]
    case class ZeState(saved: SavedState)

    class TopBackend(c: BackendScope[Props, ZeState]) {

      def tableProps = TableProps(rowpropsa(c.state.saved))

      def update1(id: Long): String => ECB2 = ???
      def update2(id: Long): String => ECB2 =
        i => ECB2[ZeState](c, ReactS.modT[IO, ZeState](s => {
          val nv = s.saved(id).i put2 i
          ???
        }))

      def revert1(id: Long): ECB2 = ???
      def revert2(id: Long): ECB2 =
        ECB2[ZeState](c, ReactS.modT[IO, ZeState](s => {
          val i = c.props.ppl(id).age.toString
          val nv = s.saved(id).i put2 i
          ???
        }))

      def rowpropsa(saved: SavedState): Vector[SavedRowProps] =
        saved.foldLeft(Vector.empty[SavedRowProps])((q,a) => q :+ rowprops1(a._1, a._2))

      def rowprops1(id: Long, s: RowState): SavedRowProps =
        SavedRowProps(id,
          EditorInput(s.i._1, "", Some(EditorCallbacks[String, ECB2](update1(id), revert1(id), ???))),
          EditorInput(s.i._2, "", Some(EditorCallbacks[String, ECB2](update2(id), revert2(id), ???))))

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

    case class SavedRowProps(key: Long, nameEI: EditorInput[String, String, ECB2], ageEI: EditorInput[String, String, ECB2])
    val savedrow = ReactComponentB[SavedRowProps]("savedrow")
      .stateless
      .render((p, _) => {
        //val (n, a) = e2.render(???)
        val n = nameE2 render p.nameEI
        val a = ageE2 render p.ageEI
        tr(key := p.key, n, a)
      })
      .build
  }

  // ******************************************************************
  object ManualExample2_consolidated_editors {
    object RowStatus
    case class Props(ppl: Map[Long, Person])
    case class RowState(i: (String,String), rowStatus: RowStatus.type)
    type SavedState = Map[Long, RowState]
    case class ZeState(saved: SavedState)

    class TopBackend(c: BackendScope[Props, ZeState]) {

      val nopEcb = ECB2.nop(c)

      def tableProps = TableProps(rowpropsa(c.state.saved))

      def update1(id: Long): String => ECB2 = ???
      def update2(id: Long): String => ECB2 =
        i => ECB2[ZeState](c, ReactS.modT[IO, ZeState](s => {
          val nv = s.saved(id).i put2 i
          ???
        }))

      def revert1(id: Long): ECB2 = ???
      def revert2(id: Long): ECB2 =
        ECB2[ZeState](c, ReactS.modT[IO, ZeState](s => {
          val i = c.props.ppl(id).age.toString
          val nv = s.saved(id).i put2 i
          ???
        }))

      def rowpropsa(saved: SavedState): Vector[SavedRowProps] =
        saved.foldLeft(Vector.empty[SavedRowProps])((q,a) => q :+ rowprops1(a._1, a._2))

      def rowprops1(id: Long, s: RowState): SavedRowProps =
        SavedRowProps(id, EditorInput(
          s.i, "",
          Some(EditorCallbacks[String \/ String, (ECB2, ECB2)](
            {
              case -\/(i) => (update1(id)(i), nopEcb)
              case \/-(i) => (nopEcb, update2(id)(i))
            },
            (revert1(id), revert2(id)),
            ???))))
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

    case class SavedRowProps(key: Long, ei: EditorInput[(String, String), String \/ String, (ECB2, ECB2)])
    val savedrow = ReactComponentB[SavedRowProps]("savedrow")
      .stateless
      .render((p, _) => {
        val (n, a) = e2.render(p.ei)
        tr(key := p.key, n, a)
      })
      .build
  }

  // ******************************************************************
  object ManualExample3_ideally_consolidated_editor {
    object RowStatus
    case class Props(ppl: Map[Long, Person])
    case class RowState(i: EditorState, rowStatus: RowStatus.type)
    type SavedState = Map[Long, RowState]
    case class ZeState(saved: SavedState)

    class TopBackend(c: BackendScope[Props, ZeState]) {

      val nopEcb = ECB2.nop(c)

      def tableProps = TableProps(rowpropsa(c.state.saved))

      def getes(id: Long): EditorState =
        c.state.saved(id).i

      def rowpropsa(saved: SavedState): Vector[SavedRowProps] =
        saved.foldLeft(Vector.empty[SavedRowProps])((q,a) => q :+ rowprops1(a._1, a._2))

      def rowprops1(id: Long, s: RowState): SavedRowProps =
        SavedRowProps(id, () => getes(id), EditorInput(
          s.i, "",
          Some(EditorCallbacks[EditorState, ECB2](
            ???,
            ???, // TODO woah, don't want to revert all fields in bulk
            ???))))
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

    case class SavedRowProps(key: Long, getes: () => EditorState, ei: EditorInput[EditorState, EditorState, ECB2])
    val savedrow = ReactComponentB[SavedRowProps]("savedrow")
      .stateless
      .render((p, _) => {
      val (n, a) = ideal_e(p.getes).render(p.ei)
      tr(key := p.key, n, a)
    })
      .build
  }
}