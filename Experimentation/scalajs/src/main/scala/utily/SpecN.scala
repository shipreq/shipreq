package utily

import japgolly.scalajs.react._
import scalaz.effect.IO
import scalaz.{Foldable, Bind, Equal}
import scalaz.syntax.bind._
import monocle.function.Field1.first
import monocle.function.Field2.second
import monocle.function.Field3.third
import monocle.std.tuple2._
import monocle.std.tuple3._
import FormStuff._
import EditorStuff._
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui.Util._
import shipreq.webapp.client.ui.{ErrorMsg, InputEvent, Editor}

// TODO use TupleExt.mapn
object SpecN {

  /**
   * This is actually just field attribute composition.
   * Single row/record.
   *
   * @tparam G "Good", meaning entire row has passed validation, row ready to be saved.
   * @tparam P "Persisted", the last saved copy of the row.
   * @tparam V "View", the type of the DOM representation.
   *
   * Field attributes + TABLE ROW-ID + [ROW-ID & STATE AWARE VALIDATORS]
   */
  case class Spec2[S, W, G, P, V, I1:Equal, C1, O1, I2:Equal, C2, O2](
        s1: AttrSpecW[S,W,P,V,I1,C1,O1], s2: AttrSpecW[S,W,P,V,I2,C2,O2],
        oo2g: ((O1, O2)) => G) {

    type II = (I1, I2)
    type OO = (O1, O2)
    type VV = (V, V)

    def initial(p: P): II = (s1 initial p, s2 initial p)

    private def fieldRenderers[M[_] : Bind : Foldable](s2mp: S => M[P],
                                                       w: W,
                                                       saveG: (S, G) => IO[S],
                                                       eL: WierdLens[M, S, S, II]) = {

      val v1 = s1.vw.fold[S => Validator[I1,C1,O1]]( _ => s1.v )( c=> rowValidator(s1.v, c, w) )
      val v2 = s2.vw.fold[S => Validator[I2,C2,O2]]( _ => s2.v )( c=> rowValidator(s2.v, c, w) )

      //def savable1(i: I) = v.correctAndValidate(i).toOption
      def savable(s: S, e: II): Option[OO] = for {
        o1 <- v1(s).correctAndValidate(e._1).toOption
        o2 <- v2(s).correctAndValidate(e._2).toOption
      } yield (o1,o2)

      // TODO This isn't able to traverse other rows, or access them by W
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S => IO[S] = s =>
        foldableToOption(eL.get(s)).flatMap(savable(s,_)).fold(IO(s))(oo => saveG(s, oo2g(oo)))

      (
        new WiredEditor[S, I1, C1, O1, M](v1, s2mp.andThen(_ map s1.p2c), eL map first[II, I1], sf),
        new WiredEditor[S, I2, C2, O2, M](v2, s2mp.andThen(_ map s2.p2c), eL map second[II, I2], sf)
      )
    }

    def forRow(w: W): Renderable[S, G, P, II, V, VV] = new Renderable[S, G, P, II, V, VV] {
      override def renderM[M[_] : Bind : Foldable]
      (eL: WierdLens[M, S, S, II], s2mp: S => M[P])
      (saveG: (S, G) => IO[S]): ComponentStateFocus[S] => M[VV] = T => {
        val s = fieldRenderers(s2mp, w, saveG, eL)
        for {
          v1 <- s._1.render(s1.editor, T)
          v2 <- s._2.render(s2.editor, T)
        } yield (v1,v2)
      }
    }
  }

  class SpecBuilder2[P, O, V, I1:Equal, C1, O1, I2:Equal, C2, O2](
      s1: AttrSpec[P,V,I1,C1,O1], s2: AttrSpec[P,V,I2,C2,O2],
      buildO: ((O1,O2)) => O) {

    type I = (I1,I2)
    type VV = (V,V)

    def mapO[OO](f: O => OO) = new SpecBuilder2(s1,s2, f compose buildO)
    def buildO[OO](f: (O1,O2) => OO) = new SpecBuilder2(s1,s2, f.tupled)

    def rowId[W] = new B2[W]

    class B2[DataId] {
      type RowId = Option[DataId]
      private type Unsaved = Option[I]
      private type Saved = Map[DataId, (P, I)]
      type S = (Saved, Unsaved)
      private def savedL = first[S, Saved]

      def uniquenessCheck[A](f: P => A) = uniqueness[S, RowId, (DataId, (P, I)), A](
        (s,ow) => savedL.get(s).toStream.filterNot(wpi => ow.fold(false)(_ == wpi._1)),
        (wpi,a) => a == f(wpi._2._1)
      )

      def ctxAwareValidators(cv1: Option[ValidateFnW[S, RowId, O1]], cv2: Option[ValidateFnW[S, RowId, O2]]) = {
        val spec = Spec2(s1.toW(cv1), s2.toW(cv2), buildO)
        new TableSpecB[DataId, O, P, I, V, VV](spec.initial).renderFn(spec.forRow)
      }
    }
  }

  // ===================================================================================================================

  /**
   * This is actually just field attribute composition.
   * Single row/record.
   *
   * @tparam G "Good", meaning entire row has passed validation, row ready to be saved.
   * @tparam P "Persisted", the last saved copy of the row.
   * @tparam V "View", the type of the DOM representation.
   *
   * Field attributes + TABLE ROW-ID + [ROW-ID & STATE AWARE VALIDATORS]
   */
  case class Spec3[S, W, G, P, V, I1:Equal, C1, O1, I2:Equal, C2, O2, I3:Equal, C3, O3](
      s1: AttrSpecW[S,W,P,V,I1,C1,O1], s2: AttrSpecW[S,W,P,V,I2,C2,O2], s3: AttrSpecW[S,W,P,V,I3,C3,O3],
      oo2g: ((O1, O2, O3)) => G) {

    type II = (I1, I2, I3)
    type OO = (O1, O2, O3)
    type VV = (V, V, V)

    def initial(p: P): II = (s1 initial p, s2 initial p, s3 initial p)

    private def fieldRenderers[M[_] : Bind : Foldable](s2mp: S => M[P],
                                                       w: W,
                                                       saveG: (S, G) => IO[S],
                                                       eL: WierdLens[M, S, S, II]) = {

      val v1 = s1.vw.fold[S => Validator[I1,C1,O1]]( _ => s1.v )( c=> rowValidator(s1.v, c, w) )
      val v2 = s2.vw.fold[S => Validator[I2,C2,O2]]( _ => s2.v )( c=> rowValidator(s2.v, c, w) )
      val v3 = s3.vw.fold[S => Validator[I3,C3,O3]]( _ => s3.v )( c=> rowValidator(s3.v, c, w) )

      //def savable1(i: I) = v.correctAndValidate(i).toOption
      def savable(s: S, e: II): Option[OO] = for {
        o1 <- v1(s).correctAndValidate(e._1).toOption
        o2 <- v2(s).correctAndValidate(e._2).toOption
        o3 <- v3(s).correctAndValidate(e._3).toOption
      } yield (o1,o2,o3)

      // TODO This isn't able to traverse other rows, or access them by W
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S => IO[S] = s =>
        foldableToOption(eL.get(s)).flatMap(savable(s,_)).fold(IO(s))(oo => saveG(s, oo2g(oo)))

      (
        new WiredEditor[S, I1, C1, O1, M](v1, s2mp.andThen(_ map s1.p2c), eL map first[II, I1], sf),
        new WiredEditor[S, I2, C2, O2, M](v2, s2mp.andThen(_ map s2.p2c), eL map second[II, I2], sf),
        new WiredEditor[S, I3, C3, O3, M](v3, s2mp.andThen(_ map s3.p2c), eL map third[II, I3], sf)
        )
    }

    def forRow(w: W): Renderable[S, G, P, II, V, VV] = new Renderable[S, G, P, II, V, VV] {
      override def renderM[M[_] : Bind : Foldable]
      (eL: WierdLens[M, S, S, II], s2mp: S => M[P])
      (saveG: (S, G) => IO[S]): ComponentStateFocus[S] => M[VV] = T => {
        val s = fieldRenderers(s2mp, w, saveG, eL)
        for {
          v1 <- s._1.render(s1.editor, T)
          v2 <- s._2.render(s2.editor, T)
          v3 <- s._3.render(s3.editor, T)
        } yield (v1,v2,v3)
      }
    }
  }

  class SpecBuilder3[P, O, V, I1:Equal, C1, O1, I2:Equal, C2, O2, I3:Equal, C3, O3](
                                                       s1: AttrSpec[P,V,I1,C1,O1], s2: AttrSpec[P,V,I2,C2,O2],s3: AttrSpec[P,V,I3,C3,O3],
                                                       buildO: ((O1,O2,O3)) => O) {

    type I = (I1,I2,I3)
    type VV = (V,V,V)

    def mapO[OO](f: O => OO) = new SpecBuilder3(s1,s2,s3, f compose buildO)
    def buildO[OO](f: (O1,O2,O3) => OO) = new SpecBuilder3(s1,s2,s3, f.tupled)

    def rowId[W] = new B2[W]

    class B2[DataId] {
      type RowId = Option[DataId]
      private type Unsaved = Option[I]
      private type Saved = Map[DataId, (P, I)]
      type S = (Saved, Unsaved)
      private def savedL = first[S, Saved]

      def uniquenessCheck[A](f: P => A) = uniqueness[S, RowId, (DataId, (P, I)), A](
        (s,ow) => savedL.get(s).toStream.filterNot(wpi => ow.fold(false)(_ == wpi._1)),
        (wpi,a) => a == f(wpi._2._1)
      )

      def ctxAwareValidators(cv1: Option[ValidateFnW[S, RowId, O1]], cv2: Option[ValidateFnW[S, RowId, O2]], cv3: Option[ValidateFnW[S, RowId, O3]]) = {
        val spec = Spec3(s1.toW(cv1), s2.toW(cv2), s3.toW(cv3), buildO)
        new TableSpecB[DataId, O, P, I, V, VV](spec.initial).renderFn(spec.forRow)
      }
    }
  }

  // ===================================================================================================================

  def SpecBuilder[P] = new {
    def apply[V, I1:Equal, C1, O1, I2:Equal, C2, O2](s1: AttrSpec[P,V,I1,C1,O1], s2: AttrSpec[P,V,I2,C2,O2]) =
      new SpecBuilder2[P, (O1,O2), V, I1, C1, O1, I2, C2, O2](s1, s2, oo=>oo)
    def apply[V, I1:Equal, C1, O1, I2:Equal, C2, O2, I3:Equal, C3, O3](s1: AttrSpec[P,V,I1,C1,O1], s2: AttrSpec[P,V,I2,C2,O2], s3: AttrSpec[P,V,I3,C3,O3]) =
      new SpecBuilder3[P, (O1,O2,O3), V, I1, C1, O1, I2, C2, O2, I3, C3, O3](s1, s2, s3, oo=>oo)
  }

}
