package shipreq.webapp.client.data

import shipreq.base.util.NonEmptyVector
import shipreq.base.util.UnivEq.Implicits._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.ScalazReact._
import shipreq.webapp.base.data._

object DataReusability {

  implicit val reusabilityProject: Reusability[Project] = Reusability.byRef

  implicit val reusabilityCustomFields: Reusability[FieldSet.CustomFields] = Reusability.byRefOrEqual


  def reusabilityVector[A](implicit r: Reusability[A]): Reusability[Vector[A]] =
    Reusability.fn((x, y) =>
      (x.length == y.length) && x.indices.forall(i => r.test(x(i), y(i))))

  def reusabilityNonEmptyVector[A: Reusability]: Reusability[NonEmptyVector[A]] =
    reusabilityVector[A].contramap(_.whole)
}
