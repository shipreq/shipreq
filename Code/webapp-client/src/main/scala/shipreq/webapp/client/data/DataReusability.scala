package shipreq.webapp.client.data

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.{NonEmptySet, NonEmptyVector}
import shipreq.webapp.base.data._
import shipreq.webapp.client.util.{Enabled, On}

object DataReusability {

  implicit val reusabilityProject: Reusability[Project] = Reusability.byRef

  implicit val reusabilityCustomFields: Reusability[FieldSet.CustomFields] = Reusability.byRefOrEqual

  def reusabilityNonEmptyVector[A: Reusability]: Reusability[NonEmptyVector[A]] =
    Reusability.by(_.whole)

  def reusabilityNonEmptySet[A: Reusability]: Reusability[NonEmptySet[A]] =
    Reusability.by(_.whole)

  implicit val reusabilityOn      = Reusability.byEqual[On]
  implicit val reusabilityEnabled = Reusability.byEqual[Enabled]
}
