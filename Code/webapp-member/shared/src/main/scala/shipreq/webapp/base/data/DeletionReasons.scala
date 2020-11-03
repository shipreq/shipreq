package shipreq.webapp.base.data

import nyaya.util.Multimap
import shipreq.base.util.TaggedTypes.TaggedInt
import shipreq.webapp.base.data.DeletionReasons.ReqApplication
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.text.Text.DeletionReason
import shipreq.webapp.base.text.Text.Equality._

final case class DeletionReasonId(value: Int) extends TaggedInt

/**
 * It's important to record the reason that requirements are deleted. The reason can be subtle, especially in complex
 * systems, and so without a justification available the data is prone to being incorrectly restored in ignorance.
 *
 * [[reasons]] is append-only so its data's indices serve as IDs.
 *
 * The vectors in [[reqApplication]] are append-only and so order shows deletion chronology.
 */
final case class DeletionReasons(reasons       : Vector[DeletionReason.NonEmptyText],
                                 reqApplication: ReqApplication) {

  def apply(id: DeletionReasonId): DeletionReason.NonEmptyText =
    reasons(id.value)

  /**
   * Obtains the most recent reason for a requirement's deletion.
   */
  def latest(id: ReqId): DeletionReason.OptionalText =
    getLatest(id).fold[DeletionReason.OptionalText](Text.empty)(_.whole)

  /**
   * Obtains the most recent reason for a requirement's deletion.
   */
  def getLatest(id: ReqId): Option[DeletionReason.NonEmptyText] =
    reqApplication(id).lastOption.flatMap(_ map apply)
}


object DeletionReasons {
  def empty: DeletionReasons =
    DeletionReasons(Vector.empty, emptyReqApplication)

  type ReqApplication = Multimap[ReqId, Vector, Option[DeletionReasonId]]

  def emptyReqApplication: ReqApplication =
    UnivEq.emptyMultimap

  implicit def equality: UnivEq[DeletionReasons] = UnivEq.derive
}