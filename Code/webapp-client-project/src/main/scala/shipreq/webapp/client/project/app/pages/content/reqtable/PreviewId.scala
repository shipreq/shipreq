package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.scalajs.react.Reusability
import shipreq.base.util.univeq.UnivEq
import shipreq.webapp.client.project.feature.EditorFeature
import shipreq.webapp.client.project.lib.DataReusability._

sealed trait PreviewId

object PreviewId {

  case class InEditor(id: EditorFeature.PreviewId) extends PreviewId
  //  case class InCI(typ: CreationInterface.Type, col: Column) extends PreviewId

  //  implicit def equalityCI: UnivEq[InCI] = UnivEq.derive
  implicit def equality: UnivEq[PreviewId] = UnivEq.derive

  implicit def reusability: Reusability[PreviewId] = Reusability.byUnivEq
}
