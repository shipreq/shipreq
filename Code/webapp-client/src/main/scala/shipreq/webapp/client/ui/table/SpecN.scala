package shipreq.webapp.client.ui.table

import japgolly.scalajs.react.ComponentStateFocus
import scalaz.effect.IO
import scalaz.{Bind, Equal}
import scalaz.syntax.bind._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui._
import shipreq.webapp.shared.validation.{ValidatePlusR, ValidatorPlus}

object SpecN {

  final case class RowSpec1[S, R, U, P, V, I1: Equal,C1,O1](s1: FieldSpecR[S,R,P,V,I1,C1,O1], buildU: (O1) ⇒ U) extends RowSpec[S, R, U, P, I1, V] {
    override def initial(p: P): I1 = s1 initial p
    private def fieldRenderers[M[_] : Bind : Optional2](s2mp: S ⇒ M[P], w: R, save: (S, U) ⇒ IO[S], iL: WeirdLens[M,S,S,I1]) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(w))
      def savable(s: S, e: I1): Option[O1] = for {
        o1 ← v1(s).correctAndValidate(e).toOption
      } yield o1
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ iL.get(s).toOption.flatMap(savable(s, _)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), iL, sf))
    }
    override def forRow(w: R): RowRenderer[S, U, P, I1, V] =
      new RowRenderer[S, U, P, I1, V] {
        override def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M,S,S,I1], s2mp: S ⇒ M[P])(save: (S,U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[V] = T ⇒ {
          val s = fieldRenderers(s2mp, w, save, iL)
          for {
            v1 ← s.render(s1.e, T)
          } yield v1
        }
      }
  }

  final class TableSpecBuider1[P, U1, V, I1: Equal,C1,O1](s1: FieldSpec[P,V,I1,C1,O1], buildU: (O1) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuider1(s1, f compose buildU)
    def buildU[U2](f: O1 ⇒ U2) = new TableSpecBuider1(s1, f)
    def dataId[D] = new TablePreSpec1[P,D,U1,V,I1,C1,O1](s1, buildU)
  }

  final class TablePreSpec1[P, D, U1, V, I1: Equal,C1,O1](s1: FieldSpec[P,V,I1,C1,O1], buildU: (O1) ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedAndUnsaved[D, P, I1]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,I1,A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]]) =
      TableSpecB default RowSpec1(s1 toR cv1,buildU)
  }

  final case class RowSpec2[S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2](s1: FieldSpecR[S,R,P,V,I1,C1,O1],s2: FieldSpecR[S,R,P,V,I2,C2,O2], buildU: ((O1,O2)) ⇒ U) extends RowSpec[S, R, U, P, (I1,I2), (V,V)] {
    override def initial(p: P): (I1,I2) = (s1 initial p,s2 initial p)
    private def fieldRenderers[M[_] : Bind : Optional2](s2mp: S ⇒ M[P], w: R, save: (S, U) ⇒ IO[S], iL: WeirdLens[M,S,S,(I1,I2)]) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(w))
      val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(w))
      def savable(s: S, e: (I1,I2)): Option[(O1,O2)] = for {
        o1 ← v1(s).correctAndValidate(e._1).toOption
        o2 ← v2(s).correctAndValidate(e._2).toOption
      } yield (o1,o2)
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ iL.get(s).toOption.flatMap(savable(s, _)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), iL.mapF(_._1)((a,b) ⇒ a put1 b), sf),
        new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), iL.mapF(_._2)((a,b) ⇒ a put2 b), sf))
    }
    override def forRow(w: R): RowRenderer[S, U, P, (I1,I2), (V,V)] =
      new RowRenderer[S, U, P, (I1,I2), (V,V)] {
        override def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M,S,S,(I1,I2)], s2mp: S ⇒ M[P])(save: (S,U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[(V,V)] = T ⇒ {
          val s = fieldRenderers(s2mp, w, save, iL)
          for {
            v1 ← s._1.render(s1.e, T)
            v2 ← s._2.render(s2.e, T)
          } yield (v1,v2)
        }
      }
  }

  final class TableSpecBuider2[P, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2], buildU: ((O1,O2)) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuider2(s1,s2, f compose buildU)
    def buildU[U2](f: (O1,O2) ⇒ U2) = new TableSpecBuider2(s1,s2, f.tupled)
    def dataId[D] = new TablePreSpec2[P,D,U1,V,I1,C1,O1,I2,C2,O2](s1,s2, buildU)
  }

  final class TablePreSpec2[P, D, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2], buildU: ((O1,O2)) ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedAndUnsaved[D, P, (I1,I2)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]]) =
      TableSpecB default RowSpec2(s1 toR cv1,s2 toR cv2,buildU)
  }

  final case class RowSpec3[S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3](s1: FieldSpecR[S,R,P,V,I1,C1,O1],s2: FieldSpecR[S,R,P,V,I2,C2,O2],s3: FieldSpecR[S,R,P,V,I3,C3,O3], buildU: ((O1,O2,O3)) ⇒ U) extends RowSpec[S, R, U, P, (I1,I2,I3), (V,V,V)] {
    override def initial(p: P): (I1,I2,I3) = (s1 initial p,s2 initial p,s3 initial p)
    private def fieldRenderers[M[_] : Bind : Optional2](s2mp: S ⇒ M[P], w: R, save: (S, U) ⇒ IO[S], iL: WeirdLens[M,S,S,(I1,I2,I3)]) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(w))
      val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(w))
      val v3 = s3.vr.fold[S ⇒ ValidatorPlus[I3,C3,O3]](_⇒ s3.v)(s3.v stateful _(w))
      def savable(s: S, e: (I1,I2,I3)): Option[(O1,O2,O3)] = for {
        o1 ← v1(s).correctAndValidate(e._1).toOption
        o2 ← v2(s).correctAndValidate(e._2).toOption
        o3 ← v3(s).correctAndValidate(e._3).toOption
      } yield (o1,o2,o3)
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ iL.get(s).toOption.flatMap(savable(s, _)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), iL.mapF(_._1)((a,b) ⇒ a put1 b), sf),
        new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), iL.mapF(_._2)((a,b) ⇒ a put2 b), sf),
        new SmartEditor[S,I3,C3,O3,M](v3, s2mp.andThen(_ map s3.p2c), iL.mapF(_._3)((a,b) ⇒ a put3 b), sf))
    }
    override def forRow(w: R): RowRenderer[S, U, P, (I1,I2,I3), (V,V,V)] =
      new RowRenderer[S, U, P, (I1,I2,I3), (V,V,V)] {
        override def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M,S,S,(I1,I2,I3)], s2mp: S ⇒ M[P])(save: (S,U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[(V,V,V)] = T ⇒ {
          val s = fieldRenderers(s2mp, w, save, iL)
          for {
            v1 ← s._1.render(s1.e, T)
            v2 ← s._2.render(s2.e, T)
            v3 ← s._3.render(s3.e, T)
          } yield (v1,v2,v3)
        }
      }
  }

  final class TableSpecBuider3[P, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3], buildU: ((O1,O2,O3)) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuider3(s1,s2,s3, f compose buildU)
    def buildU[U2](f: (O1,O2,O3) ⇒ U2) = new TableSpecBuider3(s1,s2,s3, f.tupled)
    def dataId[D] = new TablePreSpec3[P,D,U1,V,I1,C1,O1,I2,C2,O2,I3,C3,O3](s1,s2,s3, buildU)
  }

  final class TablePreSpec3[P, D, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3], buildU: ((O1,O2,O3)) ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedAndUnsaved[D, P, (I1,I2,I3)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2,I3),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]],cv3: Option[ValidatePlusR[S,R,O3]]) =
      TableSpecB default RowSpec3(s1 toR cv1,s2 toR cv2,s3 toR cv3,buildU)
  }

  final case class RowSpec4[S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4](s1: FieldSpecR[S,R,P,V,I1,C1,O1],s2: FieldSpecR[S,R,P,V,I2,C2,O2],s3: FieldSpecR[S,R,P,V,I3,C3,O3],s4: FieldSpecR[S,R,P,V,I4,C4,O4], buildU: ((O1,O2,O3,O4)) ⇒ U) extends RowSpec[S, R, U, P, (I1,I2,I3,I4), (V,V,V,V)] {
    override def initial(p: P): (I1,I2,I3,I4) = (s1 initial p,s2 initial p,s3 initial p,s4 initial p)
    private def fieldRenderers[M[_] : Bind : Optional2](s2mp: S ⇒ M[P], w: R, save: (S, U) ⇒ IO[S], iL: WeirdLens[M,S,S,(I1,I2,I3,I4)]) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(w))
      val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(w))
      val v3 = s3.vr.fold[S ⇒ ValidatorPlus[I3,C3,O3]](_⇒ s3.v)(s3.v stateful _(w))
      val v4 = s4.vr.fold[S ⇒ ValidatorPlus[I4,C4,O4]](_⇒ s4.v)(s4.v stateful _(w))
      def savable(s: S, e: (I1,I2,I3,I4)): Option[(O1,O2,O3,O4)] = for {
        o1 ← v1(s).correctAndValidate(e._1).toOption
        o2 ← v2(s).correctAndValidate(e._2).toOption
        o3 ← v3(s).correctAndValidate(e._3).toOption
        o4 ← v4(s).correctAndValidate(e._4).toOption
      } yield (o1,o2,o3,o4)
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ iL.get(s).toOption.flatMap(savable(s, _)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), iL.mapF(_._1)((a,b) ⇒ a put1 b), sf),
        new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), iL.mapF(_._2)((a,b) ⇒ a put2 b), sf),
        new SmartEditor[S,I3,C3,O3,M](v3, s2mp.andThen(_ map s3.p2c), iL.mapF(_._3)((a,b) ⇒ a put3 b), sf),
        new SmartEditor[S,I4,C4,O4,M](v4, s2mp.andThen(_ map s4.p2c), iL.mapF(_._4)((a,b) ⇒ a put4 b), sf))
    }
    override def forRow(w: R): RowRenderer[S, U, P, (I1,I2,I3,I4), (V,V,V,V)] =
      new RowRenderer[S, U, P, (I1,I2,I3,I4), (V,V,V,V)] {
        override def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M,S,S,(I1,I2,I3,I4)], s2mp: S ⇒ M[P])(save: (S,U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[(V,V,V,V)] = T ⇒ {
          val s = fieldRenderers(s2mp, w, save, iL)
          for {
            v1 ← s._1.render(s1.e, T)
            v2 ← s._2.render(s2.e, T)
            v3 ← s._3.render(s3.e, T)
            v4 ← s._4.render(s4.e, T)
          } yield (v1,v2,v3,v4)
        }
      }
  }

  final class TableSpecBuider4[P, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4], buildU: ((O1,O2,O3,O4)) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuider4(s1,s2,s3,s4, f compose buildU)
    def buildU[U2](f: (O1,O2,O3,O4) ⇒ U2) = new TableSpecBuider4(s1,s2,s3,s4, f.tupled)
    def dataId[D] = new TablePreSpec4[P,D,U1,V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4](s1,s2,s3,s4, buildU)
  }

  final class TablePreSpec4[P, D, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4], buildU: ((O1,O2,O3,O4)) ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedAndUnsaved[D, P, (I1,I2,I3,I4)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2,I3,I4),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]],cv3: Option[ValidatePlusR[S,R,O3]],cv4: Option[ValidatePlusR[S,R,O4]]) =
      TableSpecB default RowSpec4(s1 toR cv1,s2 toR cv2,s3 toR cv3,s4 toR cv4,buildU)
  }

  final case class RowSpec5[S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5](s1: FieldSpecR[S,R,P,V,I1,C1,O1],s2: FieldSpecR[S,R,P,V,I2,C2,O2],s3: FieldSpecR[S,R,P,V,I3,C3,O3],s4: FieldSpecR[S,R,P,V,I4,C4,O4],s5: FieldSpecR[S,R,P,V,I5,C5,O5], buildU: ((O1,O2,O3,O4,O5)) ⇒ U) extends RowSpec[S, R, U, P, (I1,I2,I3,I4,I5), (V,V,V,V,V)] {
    override def initial(p: P): (I1,I2,I3,I4,I5) = (s1 initial p,s2 initial p,s3 initial p,s4 initial p,s5 initial p)
    private def fieldRenderers[M[_] : Bind : Optional2](s2mp: S ⇒ M[P], w: R, save: (S, U) ⇒ IO[S], iL: WeirdLens[M,S,S,(I1,I2,I3,I4,I5)]) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(w))
      val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(w))
      val v3 = s3.vr.fold[S ⇒ ValidatorPlus[I3,C3,O3]](_⇒ s3.v)(s3.v stateful _(w))
      val v4 = s4.vr.fold[S ⇒ ValidatorPlus[I4,C4,O4]](_⇒ s4.v)(s4.v stateful _(w))
      val v5 = s5.vr.fold[S ⇒ ValidatorPlus[I5,C5,O5]](_⇒ s5.v)(s5.v stateful _(w))
      def savable(s: S, e: (I1,I2,I3,I4,I5)): Option[(O1,O2,O3,O4,O5)] = for {
        o1 ← v1(s).correctAndValidate(e._1).toOption
        o2 ← v2(s).correctAndValidate(e._2).toOption
        o3 ← v3(s).correctAndValidate(e._3).toOption
        o4 ← v4(s).correctAndValidate(e._4).toOption
        o5 ← v5(s).correctAndValidate(e._5).toOption
      } yield (o1,o2,o3,o4,o5)
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ iL.get(s).toOption.flatMap(savable(s, _)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), iL.mapF(_._1)((a,b) ⇒ a put1 b), sf),
        new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), iL.mapF(_._2)((a,b) ⇒ a put2 b), sf),
        new SmartEditor[S,I3,C3,O3,M](v3, s2mp.andThen(_ map s3.p2c), iL.mapF(_._3)((a,b) ⇒ a put3 b), sf),
        new SmartEditor[S,I4,C4,O4,M](v4, s2mp.andThen(_ map s4.p2c), iL.mapF(_._4)((a,b) ⇒ a put4 b), sf),
        new SmartEditor[S,I5,C5,O5,M](v5, s2mp.andThen(_ map s5.p2c), iL.mapF(_._5)((a,b) ⇒ a put5 b), sf))
    }
    override def forRow(w: R): RowRenderer[S, U, P, (I1,I2,I3,I4,I5), (V,V,V,V,V)] =
      new RowRenderer[S, U, P, (I1,I2,I3,I4,I5), (V,V,V,V,V)] {
        override def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M,S,S,(I1,I2,I3,I4,I5)], s2mp: S ⇒ M[P])(save: (S,U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[(V,V,V,V,V)] = T ⇒ {
          val s = fieldRenderers(s2mp, w, save, iL)
          for {
            v1 ← s._1.render(s1.e, T)
            v2 ← s._2.render(s2.e, T)
            v3 ← s._3.render(s3.e, T)
            v4 ← s._4.render(s4.e, T)
            v5 ← s._5.render(s5.e, T)
          } yield (v1,v2,v3,v4,v5)
        }
      }
  }

  final class TableSpecBuider5[P, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5], buildU: ((O1,O2,O3,O4,O5)) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuider5(s1,s2,s3,s4,s5, f compose buildU)
    def buildU[U2](f: (O1,O2,O3,O4,O5) ⇒ U2) = new TableSpecBuider5(s1,s2,s3,s4,s5, f.tupled)
    def dataId[D] = new TablePreSpec5[P,D,U1,V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4,I5,C5,O5](s1,s2,s3,s4,s5, buildU)
  }

  final class TablePreSpec5[P, D, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5], buildU: ((O1,O2,O3,O4,O5)) ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedAndUnsaved[D, P, (I1,I2,I3,I4,I5)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2,I3,I4,I5),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]],cv3: Option[ValidatePlusR[S,R,O3]],cv4: Option[ValidatePlusR[S,R,O4]],cv5: Option[ValidatePlusR[S,R,O5]]) =
      TableSpecB default RowSpec5(s1 toR cv1,s2 toR cv2,s3 toR cv3,s4 toR cv4,s5 toR cv5,buildU)
  }

  final case class RowSpec6[S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6](s1: FieldSpecR[S,R,P,V,I1,C1,O1],s2: FieldSpecR[S,R,P,V,I2,C2,O2],s3: FieldSpecR[S,R,P,V,I3,C3,O3],s4: FieldSpecR[S,R,P,V,I4,C4,O4],s5: FieldSpecR[S,R,P,V,I5,C5,O5],s6: FieldSpecR[S,R,P,V,I6,C6,O6], buildU: ((O1,O2,O3,O4,O5,O6)) ⇒ U) extends RowSpec[S, R, U, P, (I1,I2,I3,I4,I5,I6), (V,V,V,V,V,V)] {
    override def initial(p: P): (I1,I2,I3,I4,I5,I6) = (s1 initial p,s2 initial p,s3 initial p,s4 initial p,s5 initial p,s6 initial p)
    private def fieldRenderers[M[_] : Bind : Optional2](s2mp: S ⇒ M[P], w: R, save: (S, U) ⇒ IO[S], iL: WeirdLens[M,S,S,(I1,I2,I3,I4,I5,I6)]) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(w))
      val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(w))
      val v3 = s3.vr.fold[S ⇒ ValidatorPlus[I3,C3,O3]](_⇒ s3.v)(s3.v stateful _(w))
      val v4 = s4.vr.fold[S ⇒ ValidatorPlus[I4,C4,O4]](_⇒ s4.v)(s4.v stateful _(w))
      val v5 = s5.vr.fold[S ⇒ ValidatorPlus[I5,C5,O5]](_⇒ s5.v)(s5.v stateful _(w))
      val v6 = s6.vr.fold[S ⇒ ValidatorPlus[I6,C6,O6]](_⇒ s6.v)(s6.v stateful _(w))
      def savable(s: S, e: (I1,I2,I3,I4,I5,I6)): Option[(O1,O2,O3,O4,O5,O6)] = for {
        o1 ← v1(s).correctAndValidate(e._1).toOption
        o2 ← v2(s).correctAndValidate(e._2).toOption
        o3 ← v3(s).correctAndValidate(e._3).toOption
        o4 ← v4(s).correctAndValidate(e._4).toOption
        o5 ← v5(s).correctAndValidate(e._5).toOption
        o6 ← v6(s).correctAndValidate(e._6).toOption
      } yield (o1,o2,o3,o4,o5,o6)
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ iL.get(s).toOption.flatMap(savable(s, _)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), iL.mapF(_._1)((a,b) ⇒ a put1 b), sf),
        new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), iL.mapF(_._2)((a,b) ⇒ a put2 b), sf),
        new SmartEditor[S,I3,C3,O3,M](v3, s2mp.andThen(_ map s3.p2c), iL.mapF(_._3)((a,b) ⇒ a put3 b), sf),
        new SmartEditor[S,I4,C4,O4,M](v4, s2mp.andThen(_ map s4.p2c), iL.mapF(_._4)((a,b) ⇒ a put4 b), sf),
        new SmartEditor[S,I5,C5,O5,M](v5, s2mp.andThen(_ map s5.p2c), iL.mapF(_._5)((a,b) ⇒ a put5 b), sf),
        new SmartEditor[S,I6,C6,O6,M](v6, s2mp.andThen(_ map s6.p2c), iL.mapF(_._6)((a,b) ⇒ a put6 b), sf))
    }
    override def forRow(w: R): RowRenderer[S, U, P, (I1,I2,I3,I4,I5,I6), (V,V,V,V,V,V)] =
      new RowRenderer[S, U, P, (I1,I2,I3,I4,I5,I6), (V,V,V,V,V,V)] {
        override def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M,S,S,(I1,I2,I3,I4,I5,I6)], s2mp: S ⇒ M[P])(save: (S,U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[(V,V,V,V,V,V)] = T ⇒ {
          val s = fieldRenderers(s2mp, w, save, iL)
          for {
            v1 ← s._1.render(s1.e, T)
            v2 ← s._2.render(s2.e, T)
            v3 ← s._3.render(s3.e, T)
            v4 ← s._4.render(s4.e, T)
            v5 ← s._5.render(s5.e, T)
            v6 ← s._6.render(s6.e, T)
          } yield (v1,v2,v3,v4,v5,v6)
        }
      }
  }

  final class TableSpecBuider6[P, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6], buildU: ((O1,O2,O3,O4,O5,O6)) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuider6(s1,s2,s3,s4,s5,s6, f compose buildU)
    def buildU[U2](f: (O1,O2,O3,O4,O5,O6) ⇒ U2) = new TableSpecBuider6(s1,s2,s3,s4,s5,s6, f.tupled)
    def dataId[D] = new TablePreSpec6[P,D,U1,V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4,I5,C5,O5,I6,C6,O6](s1,s2,s3,s4,s5,s6, buildU)
  }

  final class TablePreSpec6[P, D, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6], buildU: ((O1,O2,O3,O4,O5,O6)) ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedAndUnsaved[D, P, (I1,I2,I3,I4,I5,I6)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2,I3,I4,I5,I6),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]],cv3: Option[ValidatePlusR[S,R,O3]],cv4: Option[ValidatePlusR[S,R,O4]],cv5: Option[ValidatePlusR[S,R,O5]],cv6: Option[ValidatePlusR[S,R,O6]]) =
      TableSpecB default RowSpec6(s1 toR cv1,s2 toR cv2,s3 toR cv3,s4 toR cv4,s5 toR cv5,s6 toR cv6,buildU)
  }

  final case class RowSpec7[S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7](s1: FieldSpecR[S,R,P,V,I1,C1,O1],s2: FieldSpecR[S,R,P,V,I2,C2,O2],s3: FieldSpecR[S,R,P,V,I3,C3,O3],s4: FieldSpecR[S,R,P,V,I4,C4,O4],s5: FieldSpecR[S,R,P,V,I5,C5,O5],s6: FieldSpecR[S,R,P,V,I6,C6,O6],s7: FieldSpecR[S,R,P,V,I7,C7,O7], buildU: ((O1,O2,O3,O4,O5,O6,O7)) ⇒ U) extends RowSpec[S, R, U, P, (I1,I2,I3,I4,I5,I6,I7), (V,V,V,V,V,V,V)] {
    override def initial(p: P): (I1,I2,I3,I4,I5,I6,I7) = (s1 initial p,s2 initial p,s3 initial p,s4 initial p,s5 initial p,s6 initial p,s7 initial p)
    private def fieldRenderers[M[_] : Bind : Optional2](s2mp: S ⇒ M[P], w: R, save: (S, U) ⇒ IO[S], iL: WeirdLens[M,S,S,(I1,I2,I3,I4,I5,I6,I7)]) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(w))
      val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(w))
      val v3 = s3.vr.fold[S ⇒ ValidatorPlus[I3,C3,O3]](_⇒ s3.v)(s3.v stateful _(w))
      val v4 = s4.vr.fold[S ⇒ ValidatorPlus[I4,C4,O4]](_⇒ s4.v)(s4.v stateful _(w))
      val v5 = s5.vr.fold[S ⇒ ValidatorPlus[I5,C5,O5]](_⇒ s5.v)(s5.v stateful _(w))
      val v6 = s6.vr.fold[S ⇒ ValidatorPlus[I6,C6,O6]](_⇒ s6.v)(s6.v stateful _(w))
      val v7 = s7.vr.fold[S ⇒ ValidatorPlus[I7,C7,O7]](_⇒ s7.v)(s7.v stateful _(w))
      def savable(s: S, e: (I1,I2,I3,I4,I5,I6,I7)): Option[(O1,O2,O3,O4,O5,O6,O7)] = for {
        o1 ← v1(s).correctAndValidate(e._1).toOption
        o2 ← v2(s).correctAndValidate(e._2).toOption
        o3 ← v3(s).correctAndValidate(e._3).toOption
        o4 ← v4(s).correctAndValidate(e._4).toOption
        o5 ← v5(s).correctAndValidate(e._5).toOption
        o6 ← v6(s).correctAndValidate(e._6).toOption
        o7 ← v7(s).correctAndValidate(e._7).toOption
      } yield (o1,o2,o3,o4,o5,o6,o7)
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ iL.get(s).toOption.flatMap(savable(s, _)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), iL.mapF(_._1)((a,b) ⇒ a put1 b), sf),
        new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), iL.mapF(_._2)((a,b) ⇒ a put2 b), sf),
        new SmartEditor[S,I3,C3,O3,M](v3, s2mp.andThen(_ map s3.p2c), iL.mapF(_._3)((a,b) ⇒ a put3 b), sf),
        new SmartEditor[S,I4,C4,O4,M](v4, s2mp.andThen(_ map s4.p2c), iL.mapF(_._4)((a,b) ⇒ a put4 b), sf),
        new SmartEditor[S,I5,C5,O5,M](v5, s2mp.andThen(_ map s5.p2c), iL.mapF(_._5)((a,b) ⇒ a put5 b), sf),
        new SmartEditor[S,I6,C6,O6,M](v6, s2mp.andThen(_ map s6.p2c), iL.mapF(_._6)((a,b) ⇒ a put6 b), sf),
        new SmartEditor[S,I7,C7,O7,M](v7, s2mp.andThen(_ map s7.p2c), iL.mapF(_._7)((a,b) ⇒ a put7 b), sf))
    }
    override def forRow(w: R): RowRenderer[S, U, P, (I1,I2,I3,I4,I5,I6,I7), (V,V,V,V,V,V,V)] =
      new RowRenderer[S, U, P, (I1,I2,I3,I4,I5,I6,I7), (V,V,V,V,V,V,V)] {
        override def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M,S,S,(I1,I2,I3,I4,I5,I6,I7)], s2mp: S ⇒ M[P])(save: (S,U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[(V,V,V,V,V,V,V)] = T ⇒ {
          val s = fieldRenderers(s2mp, w, save, iL)
          for {
            v1 ← s._1.render(s1.e, T)
            v2 ← s._2.render(s2.e, T)
            v3 ← s._3.render(s3.e, T)
            v4 ← s._4.render(s4.e, T)
            v5 ← s._5.render(s5.e, T)
            v6 ← s._6.render(s6.e, T)
            v7 ← s._7.render(s7.e, T)
          } yield (v1,v2,v3,v4,v5,v6,v7)
        }
      }
  }

  final class TableSpecBuider7[P, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6],s7: FieldSpec[P,V,I7,C7,O7], buildU: ((O1,O2,O3,O4,O5,O6,O7)) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuider7(s1,s2,s3,s4,s5,s6,s7, f compose buildU)
    def buildU[U2](f: (O1,O2,O3,O4,O5,O6,O7) ⇒ U2) = new TableSpecBuider7(s1,s2,s3,s4,s5,s6,s7, f.tupled)
    def dataId[D] = new TablePreSpec7[P,D,U1,V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4,I5,C5,O5,I6,C6,O6,I7,C7,O7](s1,s2,s3,s4,s5,s6,s7, buildU)
  }

  final class TablePreSpec7[P, D, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6],s7: FieldSpec[P,V,I7,C7,O7], buildU: ((O1,O2,O3,O4,O5,O6,O7)) ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedAndUnsaved[D, P, (I1,I2,I3,I4,I5,I6,I7)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2,I3,I4,I5,I6,I7),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]],cv3: Option[ValidatePlusR[S,R,O3]],cv4: Option[ValidatePlusR[S,R,O4]],cv5: Option[ValidatePlusR[S,R,O5]],cv6: Option[ValidatePlusR[S,R,O6]],cv7: Option[ValidatePlusR[S,R,O7]]) =
      TableSpecB default RowSpec7(s1 toR cv1,s2 toR cv2,s3 toR cv3,s4 toR cv4,s5 toR cv5,s6 toR cv6,s7 toR cv7,buildU)
  }

  final case class RowSpec8[S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7,I8: Equal,C8,O8](s1: FieldSpecR[S,R,P,V,I1,C1,O1],s2: FieldSpecR[S,R,P,V,I2,C2,O2],s3: FieldSpecR[S,R,P,V,I3,C3,O3],s4: FieldSpecR[S,R,P,V,I4,C4,O4],s5: FieldSpecR[S,R,P,V,I5,C5,O5],s6: FieldSpecR[S,R,P,V,I6,C6,O6],s7: FieldSpecR[S,R,P,V,I7,C7,O7],s8: FieldSpecR[S,R,P,V,I8,C8,O8], buildU: ((O1,O2,O3,O4,O5,O6,O7,O8)) ⇒ U) extends RowSpec[S, R, U, P, (I1,I2,I3,I4,I5,I6,I7,I8), (V,V,V,V,V,V,V,V)] {
    override def initial(p: P): (I1,I2,I3,I4,I5,I6,I7,I8) = (s1 initial p,s2 initial p,s3 initial p,s4 initial p,s5 initial p,s6 initial p,s7 initial p,s8 initial p)
    private def fieldRenderers[M[_] : Bind : Optional2](s2mp: S ⇒ M[P], w: R, save: (S, U) ⇒ IO[S], iL: WeirdLens[M,S,S,(I1,I2,I3,I4,I5,I6,I7,I8)]) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(w))
      val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(w))
      val v3 = s3.vr.fold[S ⇒ ValidatorPlus[I3,C3,O3]](_⇒ s3.v)(s3.v stateful _(w))
      val v4 = s4.vr.fold[S ⇒ ValidatorPlus[I4,C4,O4]](_⇒ s4.v)(s4.v stateful _(w))
      val v5 = s5.vr.fold[S ⇒ ValidatorPlus[I5,C5,O5]](_⇒ s5.v)(s5.v stateful _(w))
      val v6 = s6.vr.fold[S ⇒ ValidatorPlus[I6,C6,O6]](_⇒ s6.v)(s6.v stateful _(w))
      val v7 = s7.vr.fold[S ⇒ ValidatorPlus[I7,C7,O7]](_⇒ s7.v)(s7.v stateful _(w))
      val v8 = s8.vr.fold[S ⇒ ValidatorPlus[I8,C8,O8]](_⇒ s8.v)(s8.v stateful _(w))
      def savable(s: S, e: (I1,I2,I3,I4,I5,I6,I7,I8)): Option[(O1,O2,O3,O4,O5,O6,O7,O8)] = for {
        o1 ← v1(s).correctAndValidate(e._1).toOption
        o2 ← v2(s).correctAndValidate(e._2).toOption
        o3 ← v3(s).correctAndValidate(e._3).toOption
        o4 ← v4(s).correctAndValidate(e._4).toOption
        o5 ← v5(s).correctAndValidate(e._5).toOption
        o6 ← v6(s).correctAndValidate(e._6).toOption
        o7 ← v7(s).correctAndValidate(e._7).toOption
        o8 ← v8(s).correctAndValidate(e._8).toOption
      } yield (o1,o2,o3,o4,o5,o6,o7,o8)
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ iL.get(s).toOption.flatMap(savable(s, _)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), iL.mapF(_._1)((a,b) ⇒ a put1 b), sf),
        new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), iL.mapF(_._2)((a,b) ⇒ a put2 b), sf),
        new SmartEditor[S,I3,C3,O3,M](v3, s2mp.andThen(_ map s3.p2c), iL.mapF(_._3)((a,b) ⇒ a put3 b), sf),
        new SmartEditor[S,I4,C4,O4,M](v4, s2mp.andThen(_ map s4.p2c), iL.mapF(_._4)((a,b) ⇒ a put4 b), sf),
        new SmartEditor[S,I5,C5,O5,M](v5, s2mp.andThen(_ map s5.p2c), iL.mapF(_._5)((a,b) ⇒ a put5 b), sf),
        new SmartEditor[S,I6,C6,O6,M](v6, s2mp.andThen(_ map s6.p2c), iL.mapF(_._6)((a,b) ⇒ a put6 b), sf),
        new SmartEditor[S,I7,C7,O7,M](v7, s2mp.andThen(_ map s7.p2c), iL.mapF(_._7)((a,b) ⇒ a put7 b), sf),
        new SmartEditor[S,I8,C8,O8,M](v8, s2mp.andThen(_ map s8.p2c), iL.mapF(_._8)((a,b) ⇒ a put8 b), sf))
    }
    override def forRow(w: R): RowRenderer[S, U, P, (I1,I2,I3,I4,I5,I6,I7,I8), (V,V,V,V,V,V,V,V)] =
      new RowRenderer[S, U, P, (I1,I2,I3,I4,I5,I6,I7,I8), (V,V,V,V,V,V,V,V)] {
        override def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M,S,S,(I1,I2,I3,I4,I5,I6,I7,I8)], s2mp: S ⇒ M[P])(save: (S,U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[(V,V,V,V,V,V,V,V)] = T ⇒ {
          val s = fieldRenderers(s2mp, w, save, iL)
          for {
            v1 ← s._1.render(s1.e, T)
            v2 ← s._2.render(s2.e, T)
            v3 ← s._3.render(s3.e, T)
            v4 ← s._4.render(s4.e, T)
            v5 ← s._5.render(s5.e, T)
            v6 ← s._6.render(s6.e, T)
            v7 ← s._7.render(s7.e, T)
            v8 ← s._8.render(s8.e, T)
          } yield (v1,v2,v3,v4,v5,v6,v7,v8)
        }
      }
  }

  final class TableSpecBuider8[P, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7,I8: Equal,C8,O8](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6],s7: FieldSpec[P,V,I7,C7,O7],s8: FieldSpec[P,V,I8,C8,O8], buildU: ((O1,O2,O3,O4,O5,O6,O7,O8)) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuider8(s1,s2,s3,s4,s5,s6,s7,s8, f compose buildU)
    def buildU[U2](f: (O1,O2,O3,O4,O5,O6,O7,O8) ⇒ U2) = new TableSpecBuider8(s1,s2,s3,s4,s5,s6,s7,s8, f.tupled)
    def dataId[D] = new TablePreSpec8[P,D,U1,V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4,I5,C5,O5,I6,C6,O6,I7,C7,O7,I8,C8,O8](s1,s2,s3,s4,s5,s6,s7,s8, buildU)
  }

  final class TablePreSpec8[P, D, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7,I8: Equal,C8,O8](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6],s7: FieldSpec[P,V,I7,C7,O7],s8: FieldSpec[P,V,I8,C8,O8], buildU: ((O1,O2,O3,O4,O5,O6,O7,O8)) ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedAndUnsaved[D, P, (I1,I2,I3,I4,I5,I6,I7,I8)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2,I3,I4,I5,I6,I7,I8),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]],cv3: Option[ValidatePlusR[S,R,O3]],cv4: Option[ValidatePlusR[S,R,O4]],cv5: Option[ValidatePlusR[S,R,O5]],cv6: Option[ValidatePlusR[S,R,O6]],cv7: Option[ValidatePlusR[S,R,O7]],cv8: Option[ValidatePlusR[S,R,O8]]) =
      TableSpecB default RowSpec8(s1 toR cv1,s2 toR cv2,s3 toR cv3,s4 toR cv4,s5 toR cv5,s6 toR cv6,s7 toR cv7,s8 toR cv8,buildU)
  }

  final case class RowSpec9[S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7,I8: Equal,C8,O8,I9: Equal,C9,O9](s1: FieldSpecR[S,R,P,V,I1,C1,O1],s2: FieldSpecR[S,R,P,V,I2,C2,O2],s3: FieldSpecR[S,R,P,V,I3,C3,O3],s4: FieldSpecR[S,R,P,V,I4,C4,O4],s5: FieldSpecR[S,R,P,V,I5,C5,O5],s6: FieldSpecR[S,R,P,V,I6,C6,O6],s7: FieldSpecR[S,R,P,V,I7,C7,O7],s8: FieldSpecR[S,R,P,V,I8,C8,O8],s9: FieldSpecR[S,R,P,V,I9,C9,O9], buildU: ((O1,O2,O3,O4,O5,O6,O7,O8,O9)) ⇒ U) extends RowSpec[S, R, U, P, (I1,I2,I3,I4,I5,I6,I7,I8,I9), (V,V,V,V,V,V,V,V,V)] {
    override def initial(p: P): (I1,I2,I3,I4,I5,I6,I7,I8,I9) = (s1 initial p,s2 initial p,s3 initial p,s4 initial p,s5 initial p,s6 initial p,s7 initial p,s8 initial p,s9 initial p)
    private def fieldRenderers[M[_] : Bind : Optional2](s2mp: S ⇒ M[P], w: R, save: (S, U) ⇒ IO[S], iL: WeirdLens[M,S,S,(I1,I2,I3,I4,I5,I6,I7,I8,I9)]) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(w))
      val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(w))
      val v3 = s3.vr.fold[S ⇒ ValidatorPlus[I3,C3,O3]](_⇒ s3.v)(s3.v stateful _(w))
      val v4 = s4.vr.fold[S ⇒ ValidatorPlus[I4,C4,O4]](_⇒ s4.v)(s4.v stateful _(w))
      val v5 = s5.vr.fold[S ⇒ ValidatorPlus[I5,C5,O5]](_⇒ s5.v)(s5.v stateful _(w))
      val v6 = s6.vr.fold[S ⇒ ValidatorPlus[I6,C6,O6]](_⇒ s6.v)(s6.v stateful _(w))
      val v7 = s7.vr.fold[S ⇒ ValidatorPlus[I7,C7,O7]](_⇒ s7.v)(s7.v stateful _(w))
      val v8 = s8.vr.fold[S ⇒ ValidatorPlus[I8,C8,O8]](_⇒ s8.v)(s8.v stateful _(w))
      val v9 = s9.vr.fold[S ⇒ ValidatorPlus[I9,C9,O9]](_⇒ s9.v)(s9.v stateful _(w))
      def savable(s: S, e: (I1,I2,I3,I4,I5,I6,I7,I8,I9)): Option[(O1,O2,O3,O4,O5,O6,O7,O8,O9)] = for {
        o1 ← v1(s).correctAndValidate(e._1).toOption
        o2 ← v2(s).correctAndValidate(e._2).toOption
        o3 ← v3(s).correctAndValidate(e._3).toOption
        o4 ← v4(s).correctAndValidate(e._4).toOption
        o5 ← v5(s).correctAndValidate(e._5).toOption
        o6 ← v6(s).correctAndValidate(e._6).toOption
        o7 ← v7(s).correctAndValidate(e._7).toOption
        o8 ← v8(s).correctAndValidate(e._8).toOption
        o9 ← v9(s).correctAndValidate(e._9).toOption
      } yield (o1,o2,o3,o4,o5,o6,o7,o8,o9)
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ iL.get(s).toOption.flatMap(savable(s, _)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), iL.mapF(_._1)((a,b) ⇒ a put1 b), sf),
        new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), iL.mapF(_._2)((a,b) ⇒ a put2 b), sf),
        new SmartEditor[S,I3,C3,O3,M](v3, s2mp.andThen(_ map s3.p2c), iL.mapF(_._3)((a,b) ⇒ a put3 b), sf),
        new SmartEditor[S,I4,C4,O4,M](v4, s2mp.andThen(_ map s4.p2c), iL.mapF(_._4)((a,b) ⇒ a put4 b), sf),
        new SmartEditor[S,I5,C5,O5,M](v5, s2mp.andThen(_ map s5.p2c), iL.mapF(_._5)((a,b) ⇒ a put5 b), sf),
        new SmartEditor[S,I6,C6,O6,M](v6, s2mp.andThen(_ map s6.p2c), iL.mapF(_._6)((a,b) ⇒ a put6 b), sf),
        new SmartEditor[S,I7,C7,O7,M](v7, s2mp.andThen(_ map s7.p2c), iL.mapF(_._7)((a,b) ⇒ a put7 b), sf),
        new SmartEditor[S,I8,C8,O8,M](v8, s2mp.andThen(_ map s8.p2c), iL.mapF(_._8)((a,b) ⇒ a put8 b), sf),
        new SmartEditor[S,I9,C9,O9,M](v9, s2mp.andThen(_ map s9.p2c), iL.mapF(_._9)((a,b) ⇒ a put9 b), sf))
    }
    override def forRow(w: R): RowRenderer[S, U, P, (I1,I2,I3,I4,I5,I6,I7,I8,I9), (V,V,V,V,V,V,V,V,V)] =
      new RowRenderer[S, U, P, (I1,I2,I3,I4,I5,I6,I7,I8,I9), (V,V,V,V,V,V,V,V,V)] {
        override def renderM[M[_] : Bind : Optional2](iL: WeirdLens[M,S,S,(I1,I2,I3,I4,I5,I6,I7,I8,I9)], s2mp: S ⇒ M[P])(save: (S,U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[(V,V,V,V,V,V,V,V,V)] = T ⇒ {
          val s = fieldRenderers(s2mp, w, save, iL)
          for {
            v1 ← s._1.render(s1.e, T)
            v2 ← s._2.render(s2.e, T)
            v3 ← s._3.render(s3.e, T)
            v4 ← s._4.render(s4.e, T)
            v5 ← s._5.render(s5.e, T)
            v6 ← s._6.render(s6.e, T)
            v7 ← s._7.render(s7.e, T)
            v8 ← s._8.render(s8.e, T)
            v9 ← s._9.render(s9.e, T)
          } yield (v1,v2,v3,v4,v5,v6,v7,v8,v9)
        }
      }
  }

  final class TableSpecBuider9[P, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7,I8: Equal,C8,O8,I9: Equal,C9,O9](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6],s7: FieldSpec[P,V,I7,C7,O7],s8: FieldSpec[P,V,I8,C8,O8],s9: FieldSpec[P,V,I9,C9,O9], buildU: ((O1,O2,O3,O4,O5,O6,O7,O8,O9)) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuider9(s1,s2,s3,s4,s5,s6,s7,s8,s9, f compose buildU)
    def buildU[U2](f: (O1,O2,O3,O4,O5,O6,O7,O8,O9) ⇒ U2) = new TableSpecBuider9(s1,s2,s3,s4,s5,s6,s7,s8,s9, f.tupled)
    def dataId[D] = new TablePreSpec9[P,D,U1,V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4,I5,C5,O5,I6,C6,O6,I7,C7,O7,I8,C8,O8,I9,C9,O9](s1,s2,s3,s4,s5,s6,s7,s8,s9, buildU)
  }

  final class TablePreSpec9[P, D, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7,I8: Equal,C8,O8,I9: Equal,C9,O9](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6],s7: FieldSpec[P,V,I7,C7,O7],s8: FieldSpec[P,V,I8,C8,O8],s9: FieldSpec[P,V,I9,C9,O9], buildU: ((O1,O2,O3,O4,O5,O6,O7,O8,O9)) ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedAndUnsaved[D, P, (I1,I2,I3,I4,I5,I6,I7,I8,I9)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2,I3,I4,I5,I6,I7,I8,I9),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]],cv3: Option[ValidatePlusR[S,R,O3]],cv4: Option[ValidatePlusR[S,R,O4]],cv5: Option[ValidatePlusR[S,R,O5]],cv6: Option[ValidatePlusR[S,R,O6]],cv7: Option[ValidatePlusR[S,R,O7]],cv8: Option[ValidatePlusR[S,R,O8]],cv9: Option[ValidatePlusR[S,R,O9]]) =
      TableSpecB default RowSpec9(s1 toR cv1,s2 toR cv2,s3 toR cv3,s4 toR cv4,s5 toR cv5,s6 toR cv6,s7 toR cv7,s8 toR cv8,s9 toR cv9,buildU)
  }
}

import SpecN._
final class TableSpecBuilder[P] {
  def apply[V, I1: Equal,C1,O1](s1: FieldSpec[P,V,I1,C1,O1]) = new TableSpecBuider1[P,O1,V,I1,C1,O1](s1,x⇒x)
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2]) = new TableSpecBuider2[P,(O1,O2),V,I1,C1,O1,I2,C2,O2](s1,s2,x⇒x)
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3]) = new TableSpecBuider3[P,(O1,O2,O3),V,I1,C1,O1,I2,C2,O2,I3,C3,O3](s1,s2,s3,x⇒x)
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4]) = new TableSpecBuider4[P,(O1,O2,O3,O4),V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4](s1,s2,s3,s4,x⇒x)
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5]) = new TableSpecBuider5[P,(O1,O2,O3,O4,O5),V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4,I5,C5,O5](s1,s2,s3,s4,s5,x⇒x)
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6]) = new TableSpecBuider6[P,(O1,O2,O3,O4,O5,O6),V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4,I5,C5,O5,I6,C6,O6](s1,s2,s3,s4,s5,s6,x⇒x)
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6],s7: FieldSpec[P,V,I7,C7,O7]) = new TableSpecBuider7[P,(O1,O2,O3,O4,O5,O6,O7),V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4,I5,C5,O5,I6,C6,O6,I7,C7,O7](s1,s2,s3,s4,s5,s6,s7,x⇒x)
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7,I8: Equal,C8,O8](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6],s7: FieldSpec[P,V,I7,C7,O7],s8: FieldSpec[P,V,I8,C8,O8]) = new TableSpecBuider8[P,(O1,O2,O3,O4,O5,O6,O7,O8),V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4,I5,C5,O5,I6,C6,O6,I7,C7,O7,I8,C8,O8](s1,s2,s3,s4,s5,s6,s7,s8,x⇒x)
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3,I4: Equal,C4,O4,I5: Equal,C5,O5,I6: Equal,C6,O6,I7: Equal,C7,O7,I8: Equal,C8,O8,I9: Equal,C9,O9](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3],s4: FieldSpec[P,V,I4,C4,O4],s5: FieldSpec[P,V,I5,C5,O5],s6: FieldSpec[P,V,I6,C6,O6],s7: FieldSpec[P,V,I7,C7,O7],s8: FieldSpec[P,V,I8,C8,O8],s9: FieldSpec[P,V,I9,C9,O9]) = new TableSpecBuider9[P,(O1,O2,O3,O4,O5,O6,O7,O8,O9),V,I1,C1,O1,I2,C2,O2,I3,C3,O3,I4,C4,O4,I5,C5,O5,I6,C6,O6,I7,C7,O7,I8,C8,O8,I9,C9,O9](s1,s2,s3,s4,s5,s6,s7,s8,s9,x⇒x)
}
object TableSpecBuilder { def apply[P] = new TableSpecBuilder[P] }
