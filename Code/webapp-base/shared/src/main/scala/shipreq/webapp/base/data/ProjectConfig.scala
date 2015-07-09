package shipreq.webapp.base.data

import monocle.macros.Lenses
import scalaz.{-\/, Equal, \/-}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Monoidish, Must}
import shipreq.webapp.base.util.TypeclassDerivation._

object ProjectConfig {
  implicit def equality: Equal[ProjectConfig] = deriveEqual
}

@Lenses
final case class ProjectConfig(customIssueTypes: RevAnd[CustomIssueTypeIMap],
                               customReqTypes  : RevAnd[CustomReqTypeIMap],
                               fields          : RevAnd[FieldSet],
                               tags            : RevAnd[TagTree]) {
  val rev: Rev =
    customIssueTypes.rev +
    customReqTypes  .rev +
    fields          .rev +
    tags            .rev

  override def toString =
    s"ProjectConfig($rev)"

  def atag(id: ApplicableTagId): Must[ApplicableTag] =
    Must.fromOption(tags.data.get(id), s"No tag found with $id")
      .flatMap(t => t.tag match {
      case a: ApplicableTag => Must(a)
      case _                => Must.Failed(s"$t is not an ApplicableTag")
    })

  def atags[M[X] <: TraversableOnce[X]: Monoidish](ids: M[ApplicableTagId]): Must[M[ApplicableTag]] =
    Must.foldMapM(ids)(atag)

  def atags: Stream[ApplicableTag] =
    tags.data.vstream(_.tag).filterT[ApplicableTag]

  def customField[I <: CustomFieldId, D <: CustomField](id: I)(implicit d: DataIdAux[D, I]): Must[D] =
    fields.data.customFields(id).flatMap(f =>
      Must.fromOption(d.unapplyData(f), s"$id associated with wrong type: $f"))

  def customIssueType(id: CustomIssueTypeId): Must[CustomIssueType] =
    Must.fromOption(customIssueTypes.data.get(id), s"No CustomIssueType found with $id")

  lazy val customTagFields =
    fields.data.customFields.values.filterT[CustomField.Tag]

  lazy val customTextFields =
    fields.data.customFields.values.filterT[CustomField.Text]

  lazy val liveCustomTextFields =
    customTextFields.filter(_.live :: Live)

  def reqType(i: ReqTypeId): Must[ReqType] =
    i.foldId[Must[ReqType]](Must.apply, customReqTypes.data.apply)

  def reqTypeC(i: CustomReqTypeId): Must[CustomReqType] =
    reqType(i).flatMap {
      case c: CustomReqType => Must(c)
      case f                => Must.Failed(s"$f must be a CustomReqType")
    }

  lazy val reqTypes: Stream[ReqType] =
    (customReqTypes.data.values.toStream: Stream[ReqType]) append
      (StaticReqType.valueStream        : Stream[ReqType])

  lazy val reqTypesByMnemonic: Map[ReqType.Mnemonic, ReqType] =
    reqTypes.flatMap(t => t.allMnemonics.toStream.map((_, t))).toMap

  lazy val liveTagColumnDistribution =
    TagColumnDistribution(this, _.live :: Live)

  /** Keys are lowercase */
  lazy val hashRefLookupM: Map[String, HashRefTarget] = (
    atags.map(t => (t.key.value.toLowerCase, -\/(t))) append
      customIssueTypes.data.vstream(t => (t.key.value.toLowerCase, \/-(t)))
    ).toMap

  def hashRefLookup(key: String): Option[HashRefTarget] =
    hashRefLookupM.get(key.toLowerCase)

  // Finally, ensure validity
//  import japgolly.nyaya._
//  this assertSatisfies DataProp.projectConfig.all
  //  TODO Delete ↑ once confirmed that Project tightly confirmed in events etc
}
