package shipreq.webapp.base.data

import scala.runtime.AbstractFunction1
import shipreq.base.util.{Invalid, NotApplicable, Validity}
import shipreq.webapp.base.validation.Simple._

/** The set of tags that are N/A for some implicit scope.
  *
  * Includes dead tags.
  */
final case class NaTags(set    : Set[ApplicableTagId],
                        auditor: Auditor[ApplicableTag, ApplicableTag],
                       ) extends AbstractFunction1[ApplicableTagId, Validity] {

  override def apply(id: ApplicableTagId): Validity =
    Invalid.when(set contains id)
}

object NaTags {

  def none: NaTags =
    apply(Set.empty, Auditor.id)

  private[data] def forReqType(reqTypeId: ReqTypeId, config: ProjectConfig): NaTags =
    config.reqTypes.get(reqTypeId) match {
      case Some(rt) => forReqType(rt, config.tags)
      case None     => none
    }

  private[data] def forReqType(reqType: ReqType, tags: Tags): NaTags = {
    val set: Set[ApplicableTagId] =
      tags
        .applicableTagIterator()
        .filter(_.applicableReqTypes(reqType.reqTypeId) is NotApplicable)
        .map(_.id)
        .toSet

    val auditor: Auditor[ApplicableTag, ApplicableTag] =
      Auditor.test((tag: ApplicableTag) =>
        Option.when(set contains tag.id)(
          Invalidity(s"#${tag.name} is not applicable to ${reqType.mnemonic.value}s.")))

    apply(set, auditor)
  }
}
