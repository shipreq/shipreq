package shipreq.webapp.member.data.derivation

import scala.runtime.AbstractFunction1
import shipreq.base.util.{Invalid, NotApplicable, Validity}
import shipreq.webapp.base.validation.lib.Simple._
import shipreq.webapp.member.data._

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
      case Some(rt) => forReqType(rt, config)
      case None     => none
    }

  private[data] def forReqType(reqType: ReqType, config: ProjectConfig): NaTags = {

    val naFields =
      config.fields.customTagFields
        .iterator
        .filter(_.fieldReqTypeRules(reqType.reqTypeId).isNA)
        .map(_.id)
        .toSet

    val isNA: ApplicableTag => Boolean =
      t => {
        def individuallyNA =
          t.applicableReqTypes(reqType.reqTypeId) is NotApplicable

        def naByField = naFields.nonEmpty && {
          val fields = config.liveTagFieldDistribution.fieldsFor(t.id)
          fields.nonEmpty && fields.forall(naFields.contains)
        }

        individuallyNA || naByField
      }

    val set: Set[ApplicableTagId] =
      config.tags
        .applicableTagIterator()
        .filter(isNA)
        .map(_.id)
        .toSet

    val auditor: Auditor[ApplicableTag, ApplicableTag] =
      Auditor.test((tag: ApplicableTag) =>
        Option.when(set contains tag.id)(
          Invalidity(s"#${tag.name} is not applicable to ${reqType.mnemonic.value}s.")))

    apply(set, auditor)
  }
}
