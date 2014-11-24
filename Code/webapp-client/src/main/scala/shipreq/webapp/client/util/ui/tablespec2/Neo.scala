package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react._, vdom.ReactVDom.{Tag => _, _}, all._, ScalazReact._
import monocle._
import shipreq.webapp.base.validation._
import shipreq.webapp.base.validation2._
import shipreq.webapp.client.util.ui.Util._
import scala.util.Try
import scalaz.effect.IO
import scalajs.js.undefined
import scalaz._, Scalaz._
import shipreq.base.util.ScalaExt._
import ValiS._

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

  case class EditorCallbacks[-B, -C, +D](onChange: (B, C) => D,
                                     onCancel: C => D,
                                     onEditFinished: (B, C) => D) {

    @inline private def mp[X,Y,Z](f: ((B, C) => D) => ((X, Y) => Z), g: (C => D) => (Y => Z)) =
      EditorCallbacks[X, Y, Z](f(onChange), g(onCancel), f(onEditFinished))

    def mapBC[X,Y](f: X => B, g: Y => C): EditorCallbacks[X, Y, D] =
      mp(h => (b,c) => h(f(b), g(c)), g.andThen)

    def mapB[X](f: X => B): EditorCallbacks[X, C, D] = mp(h => (b,c) => h(f(b), c), identity)
    def mapC[X](f: X => C): EditorCallbacks[B, X, D] = mp(h => (b,c) => h(b, f(c)), f.andThen)
    def mapD[X](f: D => X): EditorCallbacks[B, C, X] = mp(h => (b,c) => f(h(b, c)), f.compose)

    def modB_onChange      [X <: B](f: X => X): EditorCallbacks[X, C, D] = copy(onChange       = (b,c) => onChange      (f(b), c))
    def modB_onEditFinished[X <: B](f: X => X): EditorCallbacks[X, C, D] = copy(onEditFinished = (b,c) => onEditFinished(f(b), c))
  }

  case class EditorInput[+A, -B, -C, +D](data: A,
                                     cssClass: String,
                                     editable: Option[EditorCallbacks[B, C, D]]) {

    def mapABC[X,Y,Z](f: A => X, g: Y => B, h: Z => C): EditorInput[X, Y, Z, D] =
      copy(data = f(data), editable = editable map (_.mapBC(g, h)))

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

    def compose_1[M,N,O,P,Q](t: Editor[M,N,O,P,Q]): Editor[(A,M),(B,N),(C,O),(D,P),(V,Q)] =
      Editor[(A,M),(B,N),(C,O),(D,P),(V,Q)](i => {
        val i1: EditorInput[A, B, C, D] = ???
        val i2: EditorInput[M, N, O, P] = i
          .mapA(_._2)
          .mapB[N](/* N→(B,N) */ ???)
          .mapC[O](/* O→(C,O) */ ???)
          .mapD(_._2)
        (this render i1, t render i2)
      })

    type NoB = Unit
    def compose_2[M,N,O >: C, P,Q](t: Editor[M,N,O,P,Q]): Editor[(A,M),NoB,O,(D,P),(V,Q)] =
      Editor[(A,M),NoB,O,(D,P),(V,Q)](i => {
        val i1: EditorInput[A, B, C, D] = i
          .mapA(_._1)
          .mapB[B](_ => ())
          .mapC[C](o => o)
          .mapD(_._1)
        val i2: EditorInput[M, N, O, P] = i
          .mapA(_._2)
          .mapB[N](_ => ())
          //.mapC[O](o => o)
          .mapD(_._2)
        (this render i1, t render i2)
      })

    def compose_3[M,N,O >: C, P <: D,Q](t: Editor[M,N,O,P,Q]): Editor[(A,M),NoB,O,P,(V,Q)] =
      Editor[(A,M),NoB,O,P,(V,Q)](i => {
        val i1: EditorInput[A, B, C, D] = i
          .mapA(_._1)
          .mapB[B](_ => ())
          .mapC[C](o => o)
          .mapD[P](d => d)
        val i2: EditorInput[M, N, O, P] = i
          .mapA(_._2)
          .mapB[N](_ => ())
          //.mapC[O](o => o)
          //.mapD[P](d => d)
        (this render i1, t render i2)
      })

    def compose_4[M,N,O >: C, P <: D,Q](t: Editor[M,N,O,P,Q]): Editor[(A,M),B \/ N,O,P,(V,Q)] =
      Editor[(A,M),B \/ N,O,P,(V,Q)](i => {
        val i1: EditorInput[A, B, C, D] = i
          .mapA(_._1)
          .mapB[B](-\/.apply)
          .mapC[C](o => o)
          .mapD[P](d => d)
        val i2: EditorInput[M, N, O, P] = i
          .mapA(_._2)
          .mapB[N](\/-.apply)
        //.mapC[O](o => o)
        //.mapD[P](d => d)
        (this render i1, t render i2)
      })
  }



  trait GenField[_R] {
    final type R = _R
    type V
    def lens: SimpleLens[R, V]
  }
  trait GenFieldValue[F <: GenField[_]] {
    val f: F
    val v: f.V
  }
  object GenFieldValue {
    def apply[F <: GenField[_]](_f: F)(_v: _f.V): GenFieldValue[F] =
      new GenFieldValue[F] {
        override final val f: _f.type = _f
        override final val v = _v
      }
  }

  def compose_5[I, F <: GenField[_], C,D, A1,B1,V1, A2,B2,V2 ](e1: Editor[A1, B1, C, D, V1], e2: Editor[A2, B2, C, D, V2],
                                                               a1: I => A1, a2: I => A2,
                                                                f1: F, f2: F)
                                                              (implicit w1: B1 =:= f1.V, w2: B2 =:= f2.V):
  Editor[I, GenFieldValue[F], (F,C), D, (V1,V2)] =
    Editor[I, GenFieldValue[F], (F,C), D, (V1,V2)](
      ei => {
        val i1 = ei.mapABC[A1, B1, C](a1, GenFieldValue(f1)(_), (f1, _))
        val i2 = ei.mapABC[A2, B2, C](a2, GenFieldValue(f2)(_), (f2, _))
        (e1 render i1, e2 render i2)
      })

  def compose_b[C,D, A1,B1,V1, A2,B2,V2 ](e1: Editor[A1, B1, C, D, V1], e2: Editor[A2, B2, C, D, V2]) = new {
    def pairI[F <: GenField[_]](f1: F, f2: F)(implicit w1: B1 =:= f1.V, w2: B2 =:= f2.V) =
      apply[(A1, A2), F](_._1, _._2, f1, f2)
    def apply[I, F <: GenField[_]](a1: I => A1, a2: I => A2, f1: F, f2: F)(implicit w1: B1 =:= f1.V, w2: B2 =:= f2.V)
    : Editor[I, GenFieldValue[F], (F,C), D, (V1,V2)] =
      Editor(ei => {
        val i1 = ei.mapABC[A1, B1, C](a1, GenFieldValue(f1)(_), (f1, _))
        val i2 = ei.mapABC[A2, B2, C](a2, GenFieldValue(f2)(_), (f2, _))
        (e1 render i1, e2 render i2)
      })
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
        RU.callback[Unit](e.preventDefaultIO >> e.stopPropagationIO)(IO(t.blur()))
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

  // -------------------------------------------------------------
//  object SValiExt {

    implicit final class EditorExt2[A,B,C,D,V](val e: Editor[A,B,C,D,V]) extends AnyVal {
      type Self = Editor[A, B, C, D, V]
//      def applyLiveCorrection(v: ValidatorPlus[B, _, _]): Self =
//        e.modB_onChange(v.liveCorrect)

//      def applyPostCorrection[T](v: CorrectionPart[B, T]): Self =
//        e.modB_onEditFinished(b => v.ci(v.correct(b).value))
    }

    implicit final class EditorExtV2[A,B,C,D](val e: Editor[A,B,C,D,Modifier]) extends AnyVal {
      type Self = Editor[A, B, C, D, Modifier]

      def applyInputValidation2[S](v: ValiS[S, A, _, _]): Editor[(S, A), B, C, D, Modifier] =
        validateAndDisplayError(sa => v.correctAndValidate(sa._1, sa._2).swap.toOption.map(_.toText),
          e.mapA[(S, A)](_._2))
    }

//    def composeEditorValidator[I, C, D](v: ValidatorPlus[I, _, _], e: Editor[I, I, C, D, Modifier]): Editor[I, I, C, D, Modifier] =
//      e.applyInputValidation(v)
//        .applyLiveCorrection(v)
//        .applyPostCorrection(v.cp)
//  }

//  def composeEditorValiS[S, I, C, D](v: ValiS[S, I, _, _], e: Editor[I, I, C, D, Modifier]): Editor[I, I, C, D, Modifier] =
//    e.applyInputValidation(v)
//      .applyLiveCorrection(v)
//      .applyPostCorrection(v.cp)

  // ===================================================================================================================

  object Example {

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
    type NameSW = (Map[Long, String], Long)
    type NameSWI = (NameSW, String)
    def nameUniqueness(names: Map[Long, String], id: Long, name: String): Option[VFailure] =
      (names - id).forall(_._2 != name) match {
        case true  => None
        case false => Some(uniquenessFailure("Name"))
      }
    def uniquenessFailure(fieldName: String): VFailure =
      VFailure.forField(fieldName, NonEmptyList("must be unique."))
    def tovps[S,A](f: (S,InputCorrected[A]) => Option[VFailure]): VPS[S, A, A] =
      new VPS((s,a) => f(s,a) match {
        case None    => Success(a.value)
        case Some(r) => Failure(r)
      })
    val nameUniqueVPS = tovps[NameSW, String]((a,b) => nameUniqueness(a._1, a._2, b))
    val nameV2 = nameV.toValiS[NameSW].addValidation(nameUniqueVPS)
    val nameE3 = nameE2.applyInputValidation2(nameV2)

    sealed trait PersonField extends GenField[(String, String)] {
      final def *(_v: V): GenFieldValue[PersonField] = GenFieldValue(this)(_v)
    }
    case object PersonFieldName extends PersonField {
      override type V = String
      override def lens: SimpleLens[R, V] = SimpleLens[R](_._1)(_ put1 _)
    }
    case object PersonFieldAge extends PersonField {
      override type V = String
      override def lens: SimpleLens[R, V] = SimpleLens[R](_._2)(_ put2 _)
    }
    type PersonFieldAndInput = GenFieldValue[PersonField]
    type CompositeC = (PersonField, RU)

    // 1: NameSWI, String, RU, IO[Unit], Modifier
    // 2: String,  String, RU, IO[Unit], Modifier
    type SWII = (NameSWI, String)
    val mergedE: Editor[SWII, PersonFieldAndInput, CompositeC, IO[Unit], (Modifier, Modifier)] =
      compose_5[SWII, PersonField, RU ,IO[Unit], NameSWI,String,Modifier, String,String,Modifier](
        nameE3, ageE2, _._1, _._2, PersonFieldName, PersonFieldAge)

    object ManualExample1_split_editors {
      object RowStatus
      case class Props(ppl: Map[Long, Person])
      case class RowState(i: (String,String), rowStatus: RowStatus.type)
      type SavedState = Map[Long, RowState]
      case class ZeState(saved: SavedState)
      val ZS = ReactS.FixT[IO, ZeState]

      class TopBackend(c: BackendScope[Props, ZeState]) {

        def tableProps = TableProps(rowpropsa(c.state.saved))

        def updaten(id: Long): (PersonFieldAndInput, CompositeC) => IO[Unit] =
          (b, cc) => {
            val (_,ru) = cc
            c.runState(
              ru.zoomU[ZeState] >>
                ZS.modS{ s =>
                  val i1 = s.saved(id).i
                  val i2 = b.f.lens.set(i1, b.v)
                  s.copy(saved = s.saved + (id -> RowState(i2, RowStatus)))
                }
            )
          }

        // TODO HAD A THOUGHT! Instead of 3 callback fields we should have (CallbackType, B, C) => D
        // or (CallbackType[B], C) => D

        // 90% the same as update
        def revertn(id: Long): CompositeC => IO[Unit] =
          cc => {
            val (fc,ru) = cc
            c.runState(
              ru.zoomU[ZeState] >>
                ZS.modS{ s =>
                  val p = c.props.ppl(id)
                  val i1 = s.saved(id).i
                  val i2 = fc match {
                    case PersonFieldName => i1 put1 p.name
                    case PersonFieldAge  => i1 put2 p.age.toString
                  }
                  s.copy(saved = s.saved + (id -> RowState(i2, RowStatus)))
                }
            )
          }

        def rowpropsa(saved: SavedState): Vector[SavedRowProps] = {
          val names = saved.mapValues(_.i._1)
          saved.foldLeft(Vector.empty[SavedRowProps])((q, a) => q :+ rowprops1(names, a._1, a._2))
        }

        def rowprops1(names: Map[Long, String], id: Long, s: RowState): SavedRowProps = {
          val nameswi: NameSWI = ((names, id), s.i._1)
          SavedRowProps(id,
            EditorInput(
              (nameswi, s.i._2),
              "",
              Some(EditorCallbacks[PersonFieldAndInput, CompositeC, IO[Unit]](
                updaten(id), revertn(id), ???))))
                //update1(id), revert1(id), ???))))
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

      case class SavedRowProps(key: Long, ei: EditorInput[(NameSWI, String), PersonFieldAndInput, CompositeC, IO[Unit]])
      val savedrow = ReactComponentB[SavedRowProps]("savedrow")
        .stateless
        .render((p, _) => {
        val (n, a) = mergedE.render(p.ei)
//        val n = nameE3 render p.nameEI
//        val a = ageE2 render p.ageEI
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