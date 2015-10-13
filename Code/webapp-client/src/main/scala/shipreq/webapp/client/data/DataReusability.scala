package shipreq.webapp.client.data

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra.Reusability
import shipreq.base.util.{NonEmptySet, NonEmptyVector}
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.RemoteFn
import shipreq.webapp.client.app.ui.ProjectWidgets

object DataReusability {

  implicit val reusabilityProject: Reusability[Project] = Reusability.byRef

  implicit val reusabilityProjectConfig: Reusability[ProjectConfig] = Reusability.byRef

  implicit val reusabilityProjectWidgets: Reusability[ProjectWidgets] = Reusability.byRef

  implicit val reusabilityTagTree: Reusability[TagTree] = Reusability.byRef

  implicit val reusabilityCustomFields: Reusability[FieldSet.CustomFields] = Reusability.byRefOrEqual

  def reusabilityNonEmptyVector[A: Reusability]: Reusability[NonEmptyVector[A]] =
    Reusability.by(_.whole)

  def reusabilityNonEmptySet[A: Reusability]: Reusability[NonEmptySet[A]] =
    Reusability.by(_.whole)

  implicit def reusabilityRemote[Fn <: RemoteFn.Instance] = Reusability.by((_: Fn).key)
}
