package shipreq.webapp.base.data

import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.microlibs.stdlib_ext.StdlibExt._
import monocle.macros.Lenses
import scalaz.{-\/, Equal, \/, \/-}
import scalaz.std.option.toRight
import shipreq.base.util.Applicable
import shipreq.base.util.univeq._
import shipreq.webapp.base.util.Must._
import DataImplicits._

object ProjectConfig {
  implicit lazy val equality: Equal[ProjectConfig] =
    ScalazMacros.deriveEqual

  val empty: ProjectConfig = {
    val cit = emptyDataMap(CustomIssueType)
    val rt  = ReqTypes.empty
    val fs  = FieldSet(emptyDataMap(CustomField), StaticField.values.whole)
    val ts  = Tags.empty
    ProjectConfig(cit, rt, fs, ts)
  }
}

@Lenses
final case class ProjectConfig(customIssueTypes: CustomIssueTypeIMap,
                               reqTypes        : ReqTypes,
                               fields          : FieldSet,
                               tags            : Tags) {

  val applicability: Applicability.Default =
    Applicability(fields.get(_) match {
      case Some(f) => f.applicable
      case None    => Applicable.never
    })

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

  lazy val customImpFields: List[CustomField.Implication] =
    fields.customFields.valuesIterator.filterSubType[CustomField.Implication].toList

  lazy val customTagFields: List[CustomField.Tag] =
    fields.customFields.valuesIterator.filterSubType[CustomField.Tag].toList

  lazy val customTextFields: List[CustomField.Text] =
    fields.customFields.valuesIterator.filterSubType[CustomField.Text].toList

  lazy val liveCustomTextFields =
    customTextFields.filter(_.live(this) is Live)

  lazy val liveTagFieldDistribution =
    TagFieldDistribution(this, _.live(this) is Live)

  def deadTagFieldDistribution(deadTagFilter: CustomField.Tag.Id => Boolean): TagFieldDistribution.TagIds =
    TagFieldDistribution(this, f => f.live(this) match {
      case Live => true
      case Dead => deadTagFilter(f.id)
    })

  /** Keys are lowercase */
  lazy val hashRefLookupM: Map[String, HashRefTarget] =
    ( tags.atagIterator()            .map(t => (t.key.value.toLowerCase, -\/(t))) ++
      customIssueTypes.valuesIterator.map(t => (t.key.value.toLowerCase, \/-(t)))
    ).toMap

  def hashRefLookup(key: String): Option[HashRefTarget] =
    hashRefLookupM.get(key.toLowerCase)

  def live(id: ReqTypeId): Live = reqTypes.need(id).live
}
