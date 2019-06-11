package shipreq.webapp.base.data

import japgolly.microlibs.scalaz_ext.ScalazMacros
import monocle.macros.Lenses
import scalaz.{-\/, Equal, \/-}
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.DataImplicits._

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

  @inline def applicability = fields.applicability

  def customIssueType(id: CustomIssueTypeId): CustomIssueType =
    customIssueTypes.need(id)

  lazy val liveCustomTextFields =
    fields.customTextFields.filter(_.live(this) is Live)

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

  def live(id: ReqTypeId): Live =
    reqTypes.need(id).live
}
