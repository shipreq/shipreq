package shipreq.webapp.base.data

import monocle.macros.Lenses
import scalaz.{-\/, Equal, \/-}
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Monoidish, Must}
import shipreq.webapp.base.util.TypeclassDerivation._
import DataImplicits._

object ProjectConfig {
  implicit def equality: Equal[ProjectConfig] = deriveEqual

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

  def atag(id: ApplicableTagId): Must[ApplicableTag] =
    Must.fromOption(tags.get(id), s"No tag found with $id")
      .flatMap(t => t.tag match {
      case a: ApplicableTag => Must(a)
      case _                => Must.Failed(s"$t is not an ApplicableTag")
    })

  def atags[M[X] <: TraversableOnce[X]: Monoidish](ids: M[ApplicableTagId]): Must[M[ApplicableTag]] =
    Must.foldMapM(ids)(atag)

  def atags: Stream[ApplicableTag] =
    tags.vstream(_.tag).filterT[ApplicableTag]

  def customField[I <: CustomFieldId, D <: CustomField](id: I)(implicit d: DataIdAux[D, I]): Must[D] =
    fields.customFields(id).flatMap(f =>
      Must.fromOption(d.unapplyData(f), s"$id associated with wrong type: $f"))

  def customIssueType(id: CustomIssueTypeId): Must[CustomIssueType] =
    Must.fromOption(customIssueTypes.get(id), s"No CustomIssueType found with $id")

  lazy val customTagFields =
    fields.customFields.values.filterT[CustomField.Tag]

  lazy val customTextFields =
    fields.customFields.values.filterT[CustomField.Text]

  lazy val liveCustomTextFields =
    customTextFields.filter(_.live :: Live)

  def reqType(i: ReqTypeId): Must[ReqType] =
    i.foldId[Must[ReqType]](Must.apply, customReqTypes.apply)

  def reqTypeC(i: CustomReqTypeId): Must[CustomReqType] =
    reqType(i).flatMap {
      case c: CustomReqType => Must(c)
      case f                => Must.Failed(s"$f must be a CustomReqType")
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

  // Finally, ensure validity
//  import japgolly.nyaya._
//  this assertSatisfies DataProp.projectConfig.all
  //  TODO Delete ↑ once confirmed that Project tightly confirmed in events etc
}
