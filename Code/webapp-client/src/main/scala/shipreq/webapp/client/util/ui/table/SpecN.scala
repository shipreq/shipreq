package shipreq.webapp.client.util.ui.table

import japgolly.scalajs.react.ComponentStateFocus
import japgolly.scalajs.react.ScalazReact.ReactST
import scalaz.effect.IO
import scalaz.{Bind, Equal}
import scalaz.syntax.bind._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.validation.{ValidatePlusR, ValidatorPlus}
import shipreq.webapp.client.util.ui._
import shipreq.webapp.client.util.ui.Implicits._
import TableSpec.{MultiFieldRenderer, MultiFieldRenderer2}

object SpecN {

  final case class RowSpec1[_S, R, U, P, V, I1: Equal,C1,O1](s1: FieldSpecR[_S,R,P,V,I1,C1,O1], buildU: O1 ⇒ U) extends RowSpec[_S, R, U, P, I1, V] {
    override def initial(p: P): I1 = s1 initial p
    override def forRow(r: R): MultiFieldRenderer[S, U, P, I1, V] =
      new MultiFieldRenderer[S, U, P, I1, V] {
        private final val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(r))
        private def savableI(s: S, e: I1): Option[U] = for {
          o1 ← v1(s).correctAndValidate(e).toOption
        } yield buildU(o1)
        override def prepare[M[_] : Bind : Optional2](ig: InputGatewayE[M, S, I1]): MultiFieldRenderer2[M, S, U, P, V] =
          new MultiFieldRenderer2[M, S, U, P, V] {
            override def savableU: S ⇒ Option[U] =
              s ⇒ ig.getA(s).toOption.flatMap(savableI(s, _))
            override def render(s2mp: S ⇒ M[P], save: ReactST[IO, S, Unit]): ComponentStateFocus[S] ⇒ M[V] = {
              val se1 = new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), ig, save)
              T ⇒ for {
                v1 ← se1.render(s1.e, T)
              } yield v1
            }
          }
      }
  }

  final class TableSpecBuilder1[P, U1, V, I1: Equal,C1,O1](s1: FieldSpec[P,V,I1,C1,O1], buildU: O1 ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuilder1(s1, (o1:O1) ⇒ f(buildU(o1)))
    def buildU[U2](f: O1 ⇒ U2) = new TableSpecBuilder1(s1, f)
    def dataId[D] = new TablePreSpec1[P,D,U1,V,I1,C1,O1](s1, buildU)
  }

  final class TablePreSpec1[P, D, U1, V, I1: Equal,C1,O1](s1: FieldSpec[P,V,I1,C1,O1], buildU: O1 ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedUnsaved[D, P, I1]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,I1,A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]]) =
      TableSpecB default RowSpec1(s1 toR cv1,buildU)
  }

  final case class RowSpec2[_S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2](s1: FieldSpecR[_S,R,P,V,I1,C1,O1],s2: FieldSpecR[_S,R,P,V,I2,C2,O2], buildU: (O1,O2) ⇒ U) extends RowSpec[_S, R, U, P, (I1,I2), (V,V)] {
    override def initial(p: P): (I1,I2) = (s1 initial p,s2 initial p)
    override def forRow(r: R): MultiFieldRenderer[S, U, P, (I1,I2), (V,V)] =
      new MultiFieldRenderer[S, U, P, (I1,I2), (V,V)] {
        private final val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(r))
        private final val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(r))
        private def savableI(s: S, e: (I1,I2)): Option[U] = for {
          o1 ← v1(s).correctAndValidate(e._1).toOption
          o2 ← v2(s).correctAndValidate(e._2).toOption
        } yield buildU(o1,o2)
        override def prepare[M[_] : Bind : Optional2](ig: InputGatewayE[M, S, (I1,I2)]): MultiFieldRenderer2[M, S, U, P, (V,V)] =
          new MultiFieldRenderer2[M, S, U, P, (V,V)] {
            override def savableU: S ⇒ Option[U] =
              s ⇒ ig.getA(s).toOption.flatMap(savableI(s, _))
            override def render(s2mp: S ⇒ M[P], save: ReactST[IO, S, Unit]): ComponentStateFocus[S] ⇒ M[(V,V)] = {
              val se1 = new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), ig.map(_._1)((a,b) ⇒ a put1 b), save)
              val se2 = new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), ig.map(_._2)((a,b) ⇒ a put2 b), save)
              T ⇒ for {
                v1 ← se1.render(s1.e, T)
                v2 ← se2.render(s2.e, T)
              } yield (v1,v2)
            }
          }
      }
  }

  final class TableSpecBuilder2[P, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2], buildU: (O1,O2) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuilder2(s1,s2, (o1:O1,o2:O2) ⇒ f(buildU(o1,o2)))
    def buildU[U2](f: (O1,O2) ⇒ U2) = new TableSpecBuilder2(s1,s2, f)
    def dataId[D] = new TablePreSpec2[P,D,U1,V,I1,C1,O1,I2,C2,O2](s1,s2, buildU)
  }

  final class TablePreSpec2[P, D, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2], buildU: (O1,O2) ⇒ U1) {
    type U = U1
    type R = Option[D]
    type S = SavedUnsaved[D, P, (I1,I2)]
    def uniquenessCheck[A: Equal](f: P ⇒ A) =
      TableConstraint.uniquenessT[D,P,(I1,I2),A](f)
    def tableConstraints(cv1: Option[ValidatePlusR[S,R,O1]],cv2: Option[ValidatePlusR[S,R,O2]]) =
      TableSpecB default RowSpec2(s1 toR cv1,s2 toR cv2,buildU)
  }

  final case class RowSpec3[_S, R, U, P, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3](s1: FieldSpecR[_S,R,P,V,I1,C1,O1],s2: FieldSpecR[_S,R,P,V,I2,C2,O2],s3: FieldSpecR[_S,R,P,V,I3,C3,O3], buildU: (O1,O2,O3) ⇒ U) extends RowSpec[_S, R, U, P, (I1,I2,I3), (V,V,V)] {
    override def initial(p: P): (I1,I2,I3) = (s1 initial p,s2 initial p,s3 initial p)
    override def forRow(r: R): MultiFieldRenderer[S, U, P, (I1,I2,I3), (V,V,V)] =
      new MultiFieldRenderer[S, U, P, (I1,I2,I3), (V,V,V)] {
        private final val v1 = s1.vr.fold[S ⇒ ValidatorPlus[I1,C1,O1]](_⇒ s1.v)(s1.v stateful _(r))
        private final val v2 = s2.vr.fold[S ⇒ ValidatorPlus[I2,C2,O2]](_⇒ s2.v)(s2.v stateful _(r))
        private final val v3 = s3.vr.fold[S ⇒ ValidatorPlus[I3,C3,O3]](_⇒ s3.v)(s3.v stateful _(r))
        private def savableI(s: S, e: (I1,I2,I3)): Option[U] = for {
          o1 ← v1(s).correctAndValidate(e._1).toOption
          o2 ← v2(s).correctAndValidate(e._2).toOption
          o3 ← v3(s).correctAndValidate(e._3).toOption
        } yield buildU(o1,o2,o3)
        override def prepare[M[_] : Bind : Optional2](ig: InputGatewayE[M, S, (I1,I2,I3)]): MultiFieldRenderer2[M, S, U, P, (V,V,V)] =
          new MultiFieldRenderer2[M, S, U, P, (V,V,V)] {
            override def savableU: S ⇒ Option[U] =
              s ⇒ ig.getA(s).toOption.flatMap(savableI(s, _))
            override def render(s2mp: S ⇒ M[P], save: ReactST[IO, S, Unit]): ComponentStateFocus[S] ⇒ M[(V,V,V)] = {
              val se1 = new SmartEditor[S,I1,C1,O1,M](v1, s2mp.andThen(_ map s1.p2c), ig.map(_._1)((a,b) ⇒ a put1 b), save)
              val se2 = new SmartEditor[S,I2,C2,O2,M](v2, s2mp.andThen(_ map s2.p2c), ig.map(_._2)((a,b) ⇒ a put2 b), save)
              val se3 = new SmartEditor[S,I3,C3,O3,M](v3, s2mp.andThen(_ map s3.p2c), ig.map(_._3)((a,b) ⇒ a put3 b), save)
              T ⇒ for {
                v1 ← se1.render(s1.e, T)
                v2 ← se2.render(s2.e, T)
                v3 ← se3.render(s3.e, T)
              } yield (v1,v2,v3)
            }
          }
      }
  }

  final class TableSpecBuilder3[P, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3], buildU: (O1,O2,O3) ⇒ U1) {
    def mapU[U2](f: U1 ⇒ U2) = new TableSpecBuilder3(s1,s2,s3, (o1:O1,o2:O2,o3:O3) ⇒ f(buildU(o1,o2,o3)))
    def buildU[U2](f: (O1,O2,O3) ⇒ U2) = new TableSpecBuilder3(s1,s2,s3, f)
    def dataId[D] = new TablePreSpec3[P,D,U1,V,I1,C1,O1,I2,C2,O2,I3,C3,O3](s1,s2,s3, buildU)
  }

  final class TablePreSpec3[P, D, U1, V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3], buildU: (O1,O2,O3) ⇒ U1) {
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
  def apply[V, I1: Equal,C1,O1](s1: FieldSpec[P,V,I1,C1,O1]) = new TableSpecBuilder1[P,O1,V,I1,C1,O1](s1,(o1)⇒(o1))
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2]) = new TableSpecBuilder2[P,(O1,O2),V,I1,C1,O1,I2,C2,O2](s1,s2,(o1,o2)⇒(o1,o2))
  def apply[V, I1: Equal,C1,O1,I2: Equal,C2,O2,I3: Equal,C3,O3](s1: FieldSpec[P,V,I1,C1,O1],s2: FieldSpec[P,V,I2,C2,O2],s3: FieldSpec[P,V,I3,C3,O3]) = new TableSpecBuilder3[P,(O1,O2,O3),V,I1,C1,O1,I2,C2,O2,I3,C3,O3](s1,s2,s3,(o1,o2,o3)⇒(o1,o2,o3))
}
object TableSpecBuilder { def apply[P] = new TableSpecBuilder[P] }
