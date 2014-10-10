package shipreq.webapp.client.ui.table

import japgolly.scalajs.react.ComponentStateFocus
import scalaz.effect.IO
import scalaz.{Bind, Equal}
import scalaz.syntax.bind._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui._
import shipreq.webapp.shared.validation.{ValidatePlusR, ValidatorPlus}
import TableSpec.MultiFieldRenderer

object SpecN {

  final case class RowSpec1[_S, R, U, P, V, I1: Equal,C1,O1](s1: FieldSpecR[_S,R,P,V,I1,C1,O1], buildU: (O1) ⇒ U) extends RowSpec[_S, R, U, P, I1, V] {
    override def initial(p: P): I1 = s1 initial p
    private def fieldRenderers[M[_] : Bind : Optional2](ig: InputGatewayE[M,S,I1], s2mp: S ⇒ M[P], save: (S, U) ⇒ IO[S], r: R) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(r))
      def savable(s: S, e: I1): Option[O1] = for {
        o1 ← v1(s).correctAndValidate(e).toOption
      } yield o1
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ ig.getA(s).toOption.flatMap(i ⇒ savable(s,i)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), ig, sf))
    }
    override def forRow(r: R): MultiFieldRenderer[S, U, P, I1, V] =
      new MultiFieldRenderer[S, U, P, I1, V] {
        override def render[M[_] : Bind : Optional2](ig: InputGatewayE[M,S,I1], s2mp: S ⇒ M[P], save: (S, U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[V] = T ⇒ {
          val s = fieldRenderers(ig, s2mp, save, r)
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
    type S = SavedUnsaved[D, P, I1]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,I1,A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]]) =
      TableSpecB default RowSpec1(s1 toR cv1,buildU)
  }

  final case class RowSpec2[_S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2](s1: FieldSpecR[_S,R,P,V,I1,C1,O1],s2: FieldSpecR[_S,R,P,V,I2,C2,O2], buildU: ((O1,O2)) ⇒ U) extends RowSpec[_S, R, U, P, (I1,I2), (V,V)] {
    override def initial(p: P): (I1,I2) = (s1 initial p,s2 initial p)
    private def fieldRenderers[M[_] : Bind : Optional2](ig: InputGatewayE[M,S,(I1,I2)], s2mp: S ⇒ M[P], save: (S, U) ⇒ IO[S], r: R) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(r))
      val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(r))
      def savable(s: S, e: (I1,I2)): Option[(O1,O2)] = for {
        o1 ← v1(s).correctAndValidate(e._1).toOption
        o2 ← v2(s).correctAndValidate(e._2).toOption
      } yield (o1,o2)
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ ig.getA(s).toOption.flatMap(i ⇒ savable(s,i)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), ig.map(_._1)((a,b) ⇒ a put1 b), sf),
        new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), ig.map(_._2)((a,b) ⇒ a put2 b), sf))
    }
    override def forRow(r: R): MultiFieldRenderer[S, U, P, (I1,I2), (V,V)] =
      new MultiFieldRenderer[S, U, P, (I1,I2), (V,V)] {
        override def render[M[_] : Bind : Optional2](ig: InputGatewayE[M,S,(I1,I2)], s2mp: S ⇒ M[P], save: (S, U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[(V,V)] = T ⇒ {
          val s = fieldRenderers(ig, s2mp, save, r)
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
    type S = SavedUnsaved[D, P, (I1,I2)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]]) =
      TableSpecB default RowSpec2(s1 toR cv1,s2 toR cv2,buildU)
  }

  final case class RowSpec3[_S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3](s1: FieldSpecR[_S,R,P,V,I1,C1,O1],s2: FieldSpecR[_S,R,P,V,I2,C2,O2],s3: FieldSpecR[_S,R,P,V,I3,C3,O3], buildU: ((O1,O2,O3)) ⇒ U) extends RowSpec[_S, R, U, P, (I1,I2,I3), (V,V,V)] {
    override def initial(p: P): (I1,I2,I3) = (s1 initial p,s2 initial p,s3 initial p)
    private def fieldRenderers[M[_] : Bind : Optional2](ig: InputGatewayE[M,S,(I1,I2,I3)], s2mp: S ⇒ M[P], save: (S, U) ⇒ IO[S], r: R) = {
      val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(r))
      val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(r))
      val v3 = s3.vr.fold[S ⇒ ValidatorPlus[I3,C3,O3]](_⇒ s3.v)(s3.v stateful _(r))
      def savable(s: S, e: (I1,I2,I3)): Option[(O1,O2,O3)] = for {
        o1 ← v1(s).correctAndValidate(e._1).toOption
        o2 ← v2(s).correctAndValidate(e._2).toOption
        o3 ← v3(s).correctAndValidate(e._3).toOption
      } yield (o1,o2,o3)
      // TODO This isn't able to traverse other rows, or access them by R
      // Cannot save rows that were invalid due to this one, that have just become valid.
      val sf: S ⇒ IO[S] = s ⇒ ig.getA(s).toOption.flatMap(i ⇒ savable(s,i)).fold(IO(s))(oo ⇒ save(s, buildU(oo)))
      ( new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), ig.map(_._1)((a,b) ⇒ a put1 b), sf),
        new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), ig.map(_._2)((a,b) ⇒ a put2 b), sf),
        new SmartEditor[S,I3,C3,O3,M](v3, s2mp.andThen(_ map s3.p2c), ig.map(_._3)((a,b) ⇒ a put3 b), sf))
    }
    override def forRow(r: R): MultiFieldRenderer[S, U, P, (I1,I2,I3), (V,V,V)] =
      new MultiFieldRenderer[S, U, P, (I1,I2,I3), (V,V,V)] {
        override def render[M[_] : Bind : Optional2](ig: InputGatewayE[M,S,(I1,I2,I3)], s2mp: S ⇒ M[P], save: (S, U) ⇒ IO[S]): ComponentStateFocus[S] ⇒ M[(V,V,V)] = T ⇒ {
          val s = fieldRenderers(ig, s2mp, save, r)
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
    type S = SavedUnsaved[D, P, (I1,I2,I3)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2,I3),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]],cv3: Option[ValidatePlusR[S,R,O3]]) =
      TableSpecB default RowSpec3(s1 toR cv1,s2 toR cv2,s3 toR cv3,buildU)
  }
}

import SpecN._
final class TableSpecBuilder[P] {
  def apply[V, I1: Equal,C1,O1](s1: FieldSpec[P,V,I1,C1,O1]) = new TableSpecBuider1[P,O1,V,I1,C1,O1](s1,x⇒x)
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2]) = new TableSpecBuider2[P,(O1,O2),V,I1,C1,O1,I2,C2,O2](s1,s2,x⇒x)
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3]) = new TableSpecBuider3[P,(O1,O2,O3),V,I1,C1,O1,I2,C2,O2,I3,C3,O3](s1,s2,s3,x⇒x)
}
object TableSpecBuilder { def apply[P] = new TableSpecBuilder[P] }
