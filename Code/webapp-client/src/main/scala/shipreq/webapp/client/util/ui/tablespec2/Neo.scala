            package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import monocle._
import monocle.syntax._
import shipreq.webapp.base.validation2._
import shipreq.webapp.client.util.ui.Util._
import scala.util.Try
import scalaz.effect.IO
import scalajs.js.undefined
import scalaz._, Scalaz._
import shipreq.base.util.ScalaExt._
import scala.language.reflectiveCalls


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

  sealed abstract class EditorCallback[+I] {
    final def map[X](f: I => X): EditorCallback[X] = this match {
      case OnChange(i)       => OnChange(f(i))
      case OnEditFinished(i) => OnEditFinished(f(i))
      case OnCancel          => OnCancel
    }
  }
  final case class OnChange      [I](input: I) extends EditorCallback[I]
  final case class OnEditFinished[I](input: I) extends EditorCallback[I]
  case object      OnCancel                    extends EditorCallback[Nothing]

  case class EditorCallbacks[I, C, D](t: (EditorCallback[I], C) => D) {
    def cmap [X,Y]  (f: X => I, g: Y => C)            = EditorCallbacks[X, Y, D]((x,c) => t(x map f, g(c)))
    def cmapC[X]    (f: X => C)                       = EditorCallbacks[I, X, D]((x,c) => t(x, f(c)))
    def map  [X]    (f: D => X)                       = EditorCallbacks[I, C, X](t andThenA f)
    def dimap[X,Y,Z](f: X => I, g: Y => C, h: D => Z) = EditorCallbacks[X, Y, Z]((x,c) => h(t(x map f, g(c))))

    def pmodI(f: PartialFunction[EditorCallback[I], I]): EditorCallbacks[I, C, D] =
      EditorCallbacks((i,c) => {
        val j = f.andThen(i2 => i map (_ => i2)).applyOrElse(i, identity[EditorCallback[I]])
        t(j, c)
      })

    def pmodC(f: C => PartialFunction[EditorCallback[I], C]): EditorCallbacks[I, C, D] =
      EditorCallbacks((i,c) => {
        val c2 = f(c).applyOrElse(i, (_: Any) => c)
        t(i, c2)
      })
  }

  case class EditorInput[A, B, C, D](data: A, cssClass: String,
                                     editable: Option[EditorCallbacks[B, C, D]]) {

    def mapABC[X,Y,Z](f: A => X, g: Y => B, h: Z => C): EditorInput[X,Y,Z,D] =
      copy(data = f(data), editable = editable map (_.cmap(g, h)))

    def mapA [X](f: A => X): EditorInput[X, B, C, D] = copy(data = f(data))
    def cmapB[X](f: X => B): EditorInput[A, X, C, D] = mapCallbacks(_.cmap(f, identity))
    def cmapC[X](f: X => C): EditorInput[A, B, X, D] = mapCallbacks(_ cmapC f)
    def mapD [X](f: D => X): EditorInput[A, B, C, X] = mapCallbacks(_ map f)

    def mapCallbacks[X,Y,Z](f: EditorCallbacks[B,C,D] => EditorCallbacks[X,Y,Z]): EditorInput[A,X,Y,Z] =
      copy(editable = editable map f)
  }

  case class Editor[A, B, C, D, V](render: EditorInput[A, B, C, D] => V) {
    def strengthL[X]           : Editor[(X,A), B, C, D, V] = cmapA(_._2)
    def strengthR[X]           : Editor[(A,X), B, C, D, V] = cmapA(_._1)
    def cmapA    [X](f: X => A): Editor[X,     B, C, D, V] = Editor(i => render(i mapA f))
    def mapB     [X](f: B => X): Editor[A,     X, C, D, V] = Editor(i => render(i cmapB f))
    def mapC     [X](f: C => X): Editor[A,     B, X, D, V] = Editor(i => render(i cmapC f))
    def cmapD    [X](f: X => D): Editor[A,     B, C, X, V] = Editor(i => render(i mapD f))

    def pmodB(f: PartialFunction[EditorCallback[B], B])     : Editor[A, B, C, D, V] = cmapCallbacks[B,C,D](_ pmodI f)
    def pmodC(f: C => PartialFunction[EditorCallback[B], C]): Editor[A, B, C, D, V] = cmapCallbacks[B,C,D](_ pmodC f)

    def cmapCallbacksA[X,Y,Z](f: A => EditorCallbacks[X,Y,Z] => EditorCallbacks[B,C,D]): Editor[A,X,Y,Z,V] = Editor(i => render(i.mapCallbacks(f(i.data))))
    def cmapCallbacks [X,Y,Z](f: EditorCallbacks[X,Y,Z]      => EditorCallbacks[B,C,D]): Editor[A,X,Y,Z,V] = Editor(i => render(i mapCallbacks f))

    @inline def modCallbacksA(f: A => EditorCallbacks[B, C, D] => EditorCallbacks[B, C, D]): Editor[A, B, C, D, V] = cmapCallbacksA(f)

    def compose[M, N, O>:C, P<:D, Q](t: Editor[M,N,O,P,Q]): Editor[(A,M), B\/N, O, P, (V,Q)] =
      Editor[(A,M), B\/N, O, P, (V,Q)](i => {
        val i1 = i.copy[A,B,C,D](data = i.data._1, editable = i.editable.map(_.dimap[B,C,D](-\/.apply, c⇒c, d⇒d)))
        val i2 = i.copy[M,N,O,P](data = i.data._2, editable = i.editable.map(_.dimap[N,O,P](\/-.apply, o⇒o, p⇒p)))
        (this render i1, t render i2)
      })
  }

  object Editor {
    type F = FieldSet[_, _]
    def merge2[C, D, A1, B1, V1, A2, B2, V2, FS <: FieldSet2[_, B1, B2]](fs: FS, e1: Editor[A1, B1, C, D, V1], e2: Editor[A2, B2, C, D, V2]) = new {
      def pairI = apply[(A1, A2)](_._1, _._2)
      def apply[I](a1: I => A1, a2: I => A2): Editor[I, fs.FieldValue, (fs.Field, C), D, (V1, V2)] =
        Editor(ei => {
          val i1 = ei.mapABC[A1, B1, C](a1, fs.f1 * _, (fs.f1, _))
          val i2 = ei.mapABC[A2, B2, C](a2, fs.f2 * _, (fs.f2, _))
          (e1 render i1, e2 render i2)
        })
    }
  }

  // ↑ Abstract ↑
  // ===================================================================================================================
  // ↓ Library ↓

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
            onchange  ~~> textChangeRecv(t => cb.t(OnChange(t), nopRU)),
            onblur    ~~> textChangeRecv(t => cb.t(OnEditFinished(t), nopRU)),
            onkeydown ~~> cancelOnEscape(cb.t(OnCancel, _)))
      }
    })

  def cancelOnEscape[X](f: RU => X): ReactKeyboardEventH => X =
    e => f(e.key match {
      case "Escape" => // TODO use KeyValue
        val t = e.target
        RU.callback[Unit](e.preventDefaultIO >> e.stopPropagationIO)(IO(t.blur()))
      case _ =>
        nopRU
    })

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

  implicit final class EditorExt[A,B,C,D,V](val e: Editor[A,B,C,D,V]) extends AnyVal {
    type Self = Editor[A, B, C, D, V]

    def applyLiveCorrection(v: Validator[B, _, _]): Self =
      e.pmodB { case OnChange(b) => v.liveCorrect(b) }

    def applyPostCorrection[T](v: CorrectionPart[B, T]): Self =
      e.pmodB { case OnEditFinished(b) => v.ci(v.correct_(b).value) }
  }

  implicit final class EditorExtV[A,B,C,D](val e: Editor[A,B,C,D,Modifier]) extends AnyVal {
    type Self = Editor[A, B, C, D, Modifier]

    def applyInputValidation(v: Validator[A, _, _]): Self =
      validateAndDisplayError(i => v.correctAndValidate_(i).swap.toOption.map(_.toText), e)
  }

  def composeEditorValidator[I, C, D](v: Validator[I, _, _], e: Editor[I, I, C, D, Modifier]): Editor[I, I, C, D, Modifier] =
    e.applyInputValidation(v)
      .applyLiveCorrection(v)
      .applyPostCorrection(v.cp)

  implicit final class EditorExtV2[A,B,C,D](val e: Editor[A,B,C,D,Modifier]) extends AnyVal {
    type Self = Editor[A, B, C, D, Modifier]

    def applyInputValidation2[S](v: ValidatorS[S, A, _, _]): Editor[(S, A), B, C, D, Modifier] =
      validateAndDisplayError(sa => v.correctAndValidate(sa._1, sa._2).swap.toOption.map(_.toText), e.strengthL[S])
  }

  // --------------
  // Rows and state
  // --------------

  sealed trait RowStatus
  object RowStatus {

    /** Row is in sync with the known (local) world. An edit may or may not be in progress. */
    case object Sync extends RowStatus

    /** Row is locked pending an external response to change. (Ajax in progress) */
    case object Locked extends RowStatus

    /** Failed to coordination Local change with external agent. (Ajax failure) */
    case class Failed(retry: IO[Unit]) extends RowStatus
  }

  // final case class SavedRowDP[D, P](status: RowStatus, d: D, p: P)

  object SavedRowStore {
    final case class Row[P, I](status: RowStatus, p: P, i: I)
    class RowL[P, I] {
      private[this] def l = Lenser[Row[P, I]]
      val status = l(_.status)
      val p      = l(_.p)
      val i      = l(_.i)
    }

    type SS[K, P, I] = Map[K, Row[P, I]]
    def apply[K, P, I](pi: P => I): SavedRowStore[SS[K, P, I], K, P, I] =
      new SavedRowStore(SimpleIso.dummy, new RowL, pi)
  }
  class SavedRowStore[S, K, P, I](_s: SimpleLens[S, SavedRowStore.SS[K,P,I]],
                                     _savedRow: SavedRowStore.RowL[P, I],
                                     pi: P => I) {
    final type State = S
    final type Row = SavedRowStore.Row[P, I]
    final type SS = SavedRowStore.SS[K, P, I]
    final type KP = (K, P)

    def contramap[T](f: SimpleLens[T, S]): SavedRowStore[T, K, P, I] =
      new SavedRowStore(f |-> _s, _savedRow, pi)
    
    @inline private def initRow(p: P): Row =
      SavedRowStore.Row(RowStatus.Sync, p, pi(p))

    private def __row  (k: K): SimpleLens[SS, Row]      = SimpleLens[SS](_ apply k)((m, p) => m + (k -> p))
    private def _row   (k: K): SimpleLens[S, Row]       = _s |-> __row(k)
    private def _status(k: K): SimpleLens[S, RowStatus] = _row(k) |-> _savedRow.status
    private def _i     (k: K): SimpleLens[S, I]         = _row(k) |-> _savedRow.i
    private def _p     (k: K): SimpleLens[S, P]         = _row(k) |-> _savedRow.p

    def initStateM(s: Map[K, P])         : SS = s mapValues initRow
    def initStateS(s: Seq[P], pk: P => K): SS = initStateM(s.foldLeft(Map.empty[K, P])((m, p) => m + p.mapStrengthL(pk)))
    def initStateT(s: Seq[KP])           : SS = initStateM(s.toMap)

    def remove   (k: K)              : S => S = _s.modifyF(_ - k)
    def set      (k: K, p: P)        : S => S = _s.modifyF(_ + (k -> initRow(p)))
    def setT     (kp: KP)            : S => S = set(kp._1, kp._2)
    def setStatus(k: K, r: RowStatus): S => S = _status(k).setF(r)
    
    def getAll(s: S): Stream[(RowStatus, K, P)] =
      _s.get(s).toStream.map(x => (x._2.status, x._1, x._2.p))

    def setField[X](k: K, fv: FieldSet[X, I]#FieldValue): S => S =
      (_i(k) |-> fv.f.ilens).setF(fv.v)

    def revertField(k: K, f: FieldSet[P, I]#Field): S => S =
      s => setField(k, f * f.pv(_p(k) get s))(s)

//    private[this] implicit def autoLiftEndo(f: S => S): ReactS[S, Unit] = ReactS mod f
//    def savedRemoveR(k: K): ReactS[S, Unit] = savedRemoveF(k)
//    def savedSetR   (kp: KP)    : ReactS[S, Unit] = savedSetF(kp)

    def updateIO(saveIO: KP => IO[KP]): K => ReactST[IO, S, Unit] =
      k => ReactS.modT[IO, S](s =>
        saveIO(k, _row(k).get(s).p)
          .map(setT(_)(s)))

    def deleteIO(delIO: K => IO[Unit]): K => ReactST[IO, S, Unit] =
      k => ReactS.retM(delIO(k)) >> ReactS.mod(remove(k)).lift[IO] // TODO revisit after 0.6.0
  }

  // ↑ Library ↑
  // ===================================================================================================================
  // ↓ Application ↓

  object Example {

    case class Age(value: Int)
    case class Person(id: Long, name: String, age: Age)

    val personFields = FieldSet2[Person](_.name, _.age.toString)

    val nameV: Validator[String, String, String] = ???

    val ageV =
      Validator(
        CorrectionPart.apply3[String, Option[Int]](
          _.replaceAll("\\D", ""),
          s => Try(Option(s.toInt)).getOrElse(None),
          _.fold("")(_.toString)),
        ValidationPart[Option[Int], Age](???))

    // ValidationPlus isn't helpful. LiveCorrect used in isolation from Validator
    val nameV_1: Validator[String, String, String] = nameV
    // TODO can't cmap Validator because I in invariant & needs xmap.

    val nameE = textInputEditor
    val ageE = textInputEditor

    val nameE2 = composeEditorValidator(nameV, nameE)
    val ageE2 = composeEditorValidator(ageV, ageE)

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
    val nameE3 = nameE2.applyInputValidation2(nameV2)

    type CompositeC = (personFields.Field, RU)

    // 1: NameSWI, String, RU, IO[Unit], Modifier
    // 2: String,  String, RU, IO[Unit], Modifier
    type SWII = (NameSWI, String)
    val mergedE: Editor[SWII, personFields.FieldValue, CompositeC, IO[Unit], (Modifier, Modifier)] =
      Editor.merge2(personFields, nameE3, ageE2).pairI

    object ManualExample1_split_editors {

      case class Props(ppl: Map[Long, Person])
      
      val savedStore = SavedRowStore[Long, Person, (String, String)](p => (p.name, p.age.toString))
      case class ZeState(saved: savedStore.State)
      val savedStoreZ = savedStore.contramap(SimpleLens[ZeState](_.saved)((a,b) =>  a.copy(saved = b)))
      val ZS = ReactS.FixT[IO, ZeState]

      def updatex(id: Long, b: personFields.FieldValue) =
        ZS.modS(savedStoreZ.setField(id, b))

      def revertx(id: Long, f: personFields.Field) =
        ZS.modS(savedStoreZ.revertField(id, f))

      def validaterow(ss: NameSW, id: Long, ok: ((String, Age)) => ReactST[IO, ZeState, Unit], ko: VFailure => ReactST[IO, ZeState, Unit]) =
        ZS.liftR{ s =>
          val i = s.saved(id).i
          personVF(ss, i) match {
            case scalaz.Success(v) => ok(v)
            case scalaz.Failure(f) => ko(f)
          }
        }

      def lockrow(id: Long) =
        ZS.modS(savedStoreZ.setStatus(id, RowStatus.Locked))

      type CompositeC2 = (personFields.Field, ReactST[IO, ZeState, Unit])
      val mergedE2 = mergedE
        .strengthR[Long]
        .mapC(_ map2 (_.zoomU[ZeState]))
        .modCallbacksA(a => _.pmodC(c => {
          case OnChange(b) => c map2 (_ >> updatex(a._2, b))
          case OnCancel    => c map2 (_ >> revertx(a._2, c._1))
        }))

      val ageV2 = ageV.liftS[NameSW]
      val personV = nameV2 *** ageV2
      def personVF(s: NameSW, i: (String,String)) = personV.correctAndValidate(s, i)
      val mergedE3 = mergedE2.modCallbacksA(a => {
        val (((namesw, i1), i2), id) = a
        _.pmodC(c => {
          case OnEditFinished(b) => c map2 (_ >> validaterow(namesw, id, v => lockrow(id), _ => ZS.ret(())))
        })
      })

      class TopBackend(c: BackendScope[Props, ZeState]) {

        val cbRealise: (Any,CompositeC2) => IO[Unit] = (_,x) => c.runState(x._2)
        val editable = Some(EditorCallbacks[personFields.FieldValue, CompositeC2, IO[Unit]](cbRealise))

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