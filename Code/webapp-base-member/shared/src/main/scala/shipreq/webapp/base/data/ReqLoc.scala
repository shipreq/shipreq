package shipreq.webapp.base.data

import japgolly.univeq.UnivEq

/** Location of a tag in a requirement. */
sealed trait ReqTagLoc

object ReqTagLoc {
  case object Tags extends ReqTagLoc
  implicit def univEq: UnivEq[ReqTagLoc] = UnivEq.derive
}

/** Location of (rich) text in a requirement. */
sealed trait ReqTextLoc
  extends ReqTagLoc // more efficient than ReqTagLoc.Text(ReqTextLoc)

object ReqTextLoc {
  case object Title                                              extends ReqTextLoc
  final case class CustomTextField(fieldId: CustomField.Text.Id) extends ReqTextLoc
  final case class UseCaseStep    (stepId: UseCaseStepId)        extends ReqTextLoc

  implicit def univEq: UnivEq[ReqTextLoc] = UnivEq.derive
}
