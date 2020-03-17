package shipreq.webapp.base.data

import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.microlibs.utils.{BiMap, Memo}
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

  // ==========================================================================
  // Content

  def reqFilter(fd: FilterDead): Req => Boolean =
    fd.filterFnBy((_: Req).live(reqTypes))

  // ==========================================================================
  // HashRefs

  /** Keys are lowercase */
  lazy val hashRefLookupM: Map[String, HashRefTarget] =
    ( tags.applicableTagIterator()   .map(t => (t.key.value.toLowerCase, -\/(t))) ++
      customIssueTypes.valuesIterator.map(t => (t.key.value.toLowerCase, \/-(t)))
    ).toMap

  def hashRefLookup(key: String): Option[HashRefTarget] =
    hashRefLookupM.get(key.toLowerCase)

  // ==========================================================================
  // Fields

  val customFieldNonUniqueName: CustomField => String = {
    case f: CustomField.Text        => f.name
    case f: CustomField.Tag         => f.name(tags.tree)
    case f: CustomField.Implication => f.name(reqTypes)
  }

  lazy val fieldsByName: Map[String, Field] = {
    var m: Map[String, Field] = StaticField.byName
    fields.customFields.valuesIterator.foreach { f =>
      val name = customFieldNonUniqueName(f)

      val uniqueName = {
        var i = 2
        var n = name
        while (m.contains(name)) {
          n = name + i
          i += 1
        }
        n
      }

      m = m.updated(uniqueName, f)
    }
    m
  }

  lazy val fieldName: FieldId => String = {
    val m: Map[FieldId, String] = fieldsByName.map(x => (x._2.fieldId, x._1))
    m.apply
  }

  lazy val liveCustomTextFields: List[CustomField.Text] =
    fields.customTextFields.filter(_.live(this) is Live)

  lazy val liveOrderedFieldIds: Vector[FieldId] =
    fields.order.filter(fields.need(_).live(this) is Live)

  lazy val mandatoryLiveCustomFields: CustomField.Lists = {
    val m = new CustomField.MutableLists
    for (f <- fields.customFields.valuesIterator)
      if (f.mandatory.is(Mandatory) && f.live(this).is(Live))
        m += f
    m.result()
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

  // ==========================================================================
  // Req types

  def live(id: ReqTypeId): Live =
    reqTypes.need(id).live

  // ==========================================================================
  // Tags

  def deadTagFieldDistribution(deadTagFilter: CustomField.Tag.Id => Boolean): TagFieldDistribution.TagIds =
    TagFieldDistribution(this, f => f.live(this) match {
      case Live => true
      case Dead => deadTagFilter(f.id)
    })

  lazy val liveTagFieldDistribution: TagFieldDistribution.TagIds =
    TagFieldDistribution(this, _.live(this) is Live)

}
