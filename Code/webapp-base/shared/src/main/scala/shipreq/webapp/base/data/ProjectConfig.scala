package shipreq.webapp.base.data

import monocle.macros.Lenses
import scalaz.{\/, -\/, \/-, Equal}
import scalaz.std.option.toRight
import shipreq.base.util.ScalaExt._
import shipreq.base.util.UtilMacros
import shipreq.webapp.base.util.Must._
import DataImplicits._

object ProjectConfig {
  implicit lazy val equality: Equal[ProjectConfig] = UtilMacros.deriveEqual

  val empty: ProjectConfig = {
    val cit = emptyDataMap(CustomIssueType)
    val crt = emptyDataMap(CustomReqType)
    val fs  = FieldSet(emptyDataMap(CustomField), StaticField.values.whole)
    val tt  = TagTree.empty
    ProjectConfig(cit, crt, fs, tt)
  }
}

@Lenses
final case class ProjectConfig(customIssueTypes: CustomIssueTypeIMap,
                               customReqTypes  : CustomReqTypeIMap,
                               fields          : FieldSet,
                               tags            : TagTree) {

  def atagValidate(id: ApplicableTagId): Option[String] =
    tags.get(id) match {
      case Some(tit) => tit.tag match {
        case _: ApplicableTag => None
        case t: TagGroup      => Some(s"$t is not an ApplicableTag.")
      }
      case None               => Some(s"$id not found.")
    }

  def atag(id: ApplicableTagId): ApplicableTag =
    tags.need(id).tag match {
      case a: ApplicableTag => a
      case t: TagGroup      => mustNotHappen(s"$t is not an ApplicableTag.")
    }

  def atags: Stream[ApplicableTag] =
    tags.vstream(_.tag).filterT[ApplicableTag]

  lazy val deadATagIds: Set[ApplicableTagId] =
    atags.filter(_.live :: Dead).map(_.id).toSet

  def customField[I <: CustomFieldId, D <: CustomField](id: I)(implicit d: DataIdAux[D, I]): D = {
    val f = fields.customFields.need(id)
    d.unapplyData(f) mustExistElse s"$id associated with wrong type: $f"
  }

  def customFieldAttempt[I <: CustomFieldId, D <: CustomField](id: I)(implicit d: DataIdAux[D, I]): String \/ D =
    fields.customFields.get(id) match {
      case Some(f) =>
        toRight(d unapplyData f)(s"$id associated with wrong type: $f")
      case None =>
        -\/(s"$id not found.")
    }

  def customIssueType(id: CustomIssueTypeId): CustomIssueType =
    customIssueTypes.need(id)

  lazy val customTagFields =
    fields.customFields.values.filterT[CustomField.Tag]

  lazy val customTextFields =
    fields.customFields.values.filterT[CustomField.Text]

  lazy val liveCustomTextFields =
    customTextFields.filter(_.live :: Live)

  def reqType(i: ReqTypeId): ReqType =
    i.foldId[ReqType](identity, customReqTypes.need)

  def reqTypeC(i: CustomReqTypeId): CustomReqType =
    reqType(i) match {
      case c: CustomReqType => c
      case f                => mustNotHappen(s"$f must be a CustomReqType")
    }

  lazy val liveCustomReqTypes: Stream[CustomReqType] =
    customReqTypes.values.toStream.filter(_.live :: Live)

  lazy val reqTypes: Stream[ReqType] =
    (customReqTypes.values.toStream: Stream[ReqType]) append
      (StaticReqType.valueStream   : Stream[ReqType])

  lazy val reqTypesByMnemonic: Map[ReqType.Mnemonic, ReqType] =
    reqTypes.flatMap(t => t.allMnemonics.toStream.map((_, t))).toMap

  lazy val liveTagColumnDistribution =
    TagColumnDistribution(this, _.live :: Live)

  /** Keys are lowercase */
  lazy val hashRefLookupM: Map[String, HashRefTarget] = (
    atags.map(t => (t.key.value.toLowerCase, -\/(t))) append
      customIssueTypes.vstream(t => (t.key.value.toLowerCase, \/-(t)))
    ).toMap

  def hashRefLookup(key: String): Option[HashRefTarget] =
    hashRefLookupM.get(key.toLowerCase)
}
