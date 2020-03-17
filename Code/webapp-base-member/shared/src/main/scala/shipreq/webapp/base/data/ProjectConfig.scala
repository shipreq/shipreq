package shipreq.webapp.base.data

import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.microlibs.utils.Memo
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

  lazy val liveTagFieldDistribution: TagFieldDistribution.TagIds =
    TagFieldDistribution(this, _.live(this) is Live)

  def deadTagFieldDistribution(deadTagFilter: CustomField.Tag.Id => Boolean): TagFieldDistribution.TagIds =
    TagFieldDistribution(this, f => f.live(this) match {
      case Live => true
      case Dead => deadTagFilter(f.id)
    })

  /** Keys are lowercase */
  lazy val hashRefLookupM: Map[String, HashRefTarget] =
    ( tags.applicableTagIterator()   .map(t => (t.key.value.toLowerCase, -\/(t))) ++
      customIssueTypes.valuesIterator.map(t => (t.key.value.toLowerCase, \/-(t)))
    ).toMap

  def hashRefLookup(key: String): Option[HashRefTarget] =
    hashRefLookupM.get(key.toLowerCase)

  def live(id: ReqTypeId): Live =
    reqTypes.need(id).live

  def reqFilter(fd: FilterDead): Req => Boolean =
    fd.filterFnBy((_: Req).live(reqTypes))

  lazy val mandatoryLiveCustomFields: CustomField.Lists = {
    val m = new CustomField.MutableLists
    for (f <- fields.customFields.valuesIterator)
      if (f.mandatory.is(Mandatory) && f.live(this).is(Live))
        m += f
    m.result()
  }

  lazy val fieldName: Field => String =
    Field.name(reqTypes, tags.tree)

  lazy val fieldNameById: FieldId => String = {
    val f = fieldName
    _.foldId(f, id => f(fields.customFields.need(id)))
  }

  val mostRelevantLiveFieldForTag: TagId => Option[CustomField.Tag] =
    Memo { tagId =>
      type R = Option[CustomField.Tag]

      implicit val tree = tags.tree

      def liveTagFields = fields.customTagFields.iterator.filter(_.live(this).is(Live))

      val direct: R = liveTagFields.find(_.tagId ==* tagId)

      def soleParent: R = {
        liveTagFields.filter(f => tree.need(f.tagId).transitiveChildren.contains(f.tagId)).take(2).toList match {
          case f :: Nil => Some(f)
          case _        => None
        }
      }

      direct.orElse(soleParent)
    }
}
