package shipreq.webapp.client.data

import shipreq.base.util.UnivEq.Implicits._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.ScalazReact._
import shipreq.webapp.base.data._

object DataReusability {

  implicit val reusabilityProject: Reusability[Project] = Reusability.byRef

  implicit val reusabilityCustomFields: Reusability[FieldSet.CustomFields] = Reusability.byRefOrEqual

}
