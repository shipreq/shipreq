package shipreq.webapp.base.data

import japgolly.microlibs.scalaz_ext.ScalazMacros
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.utils.Memo
import monocle.macros.Lenses
import nyaya.util.Multimap
import scalaz.Equal
import shipreq.base.util.{Applicable, NotApplicable}
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data.derivation._

object ProjectConfig {
  implicit lazy val equality: Equal[ProjectConfig] =
    ScalazMacros.deriveEqual

  val empty: ProjectConfig = {
    val cit = emptyDataMap(CustomIssueType)
    val rt  = ReqTypes.empty
    val fs  = FieldSet.empty
    val ts  = Tags.empty
    ProjectConfig(cit, rt, fs, ts)
  }

  sealed trait TagFieldIssue
  object TagFieldIssue {
    final case class DefaultTagDead(tag: ApplicableTag) extends TagFieldIssue
    final case class DefaultTagNotApplicable(tag: ApplicableTag, reqType: ReqType) extends TagFieldIssue
    final case class DefaultTagUnrelated(tag: ApplicableTag) extends TagFieldIssue
  }

  final case class FixedRules[+D, +E](original: FieldReqTypeRules[D],
                                      fixed   : FieldReqTypeRules[D],
                                      errors  : Map[Option[ReqTypeId], E])

  object FixedRules {
    def id[D](r: FieldReqTypeRules[D]): FixedRules[D, Nothing] =
      apply(r, r, Map.empty)
  }

  private[ProjectConfig] lazy val fieldNamesLowercase: Set[String] =
    StaticField.namesLowercase ++ SpecialBuiltInField.namesLowercase
}

@Lenses
final case class ProjectConfig(customIssueTypes: CustomIssueTypeIMap,
                               reqTypes        : ReqTypes,
                               fields          : FieldSet,
                               tags            : Tags) {

  import ProjectConfig.{FixedRules, TagFieldIssue}

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
    var seenLowercase: Set[String]        = ProjectConfig.fieldNamesLowercase
    var results      : Map[String, Field] = StaticField.byName

    fields.customFields.valuesIterator.foreach { f =>
      val name = customFieldNonUniqueName(f)

      val uniqueName = {
        var i = 2
        var n = name
        while (seenLowercase.contains(n.toLowerCase)) {
          n = name + i
          i += 1
        }
        n
      }

      seenLowercase += uniqueName.toLowerCase
      results = results.updated(uniqueName, f)
    }

    results
  }

  lazy val fieldsByNameLowercaseWithFilterAliases: Map[String, Field] = {
    val norm: String => String = _.toLowerCase

    var m = fieldsByName.mapKeysNow(norm)

    // Add aliases that can be used in filter expressions
    fields.customFields.valuesIterator.foreach {
      case f: CustomField.Implication =>
        val alias = norm(reqTypes.need(f.reqTypeId).mnemonic.value)
        if (!m.contains(alias))
          m = m.updated(alias, f)
      case _ =>
        ()
    }

    m
  }

  lazy val fieldName: FieldId => String = {
    val m: Map[FieldId, String] = fieldsByName.map(x => (x._2.fieldId, x._1))
    m.apply
  }

  lazy val liveCustomFields: List[CustomField] =
    fields.customFields.valuesIterator.filter(_.live(this) is Live).toList

  lazy val liveCustomTextFields: List[CustomField.Text] =
    fields.customTextFields.filter(_.live(this) is Live)

  lazy val liveCustomTextFieldIdSet: Set[CustomField.Text.Id] =
    liveCustomTextFields.map(_.id).toSet

  lazy val liveCustomTagFields: List[CustomField.Tag] =
    fields.customTagFields.filter(_.live(this) is Live)

  lazy val liveOrderedFieldIds: Vector[FieldId] =
    fields.order.filter(fields.need(_).live(this) is Live)

  lazy val liveCustomFieldsWithMandatory: CustomField.Lists = {
    val m = new CustomField.MutableLists
    for (f <- fields.customFields.valuesIterator)
      if (f.fieldReqTypeRules.containsMandatory && f.live(this).is(Live))
        m += f
    m.result()
  }

  /** - Dead req types are ignored / unmodified
    * - Rules that default to dead tags are replaced with Optional
    * - Rules that default to unrelated tags are replaced with Optional
    * - Rules that default to non-applicable tags are replaced with Optional
    */
  val tagFieldRulesFixedHideDead: CustomField.Tag.Id => FixedRules[ApplicableTagId, TagFieldIssue] =
    Memo { fieldId =>
      val field = fields.custom(fieldId)
      if (field.live(this) is Dead)
        FixedRules.id(field.fieldReqTypeRules)
      else {
        val original = field.fieldReqTypeRules
        var fixed    = original
        var errors   = Map.empty[Option[ReqTypeId], TagFieldIssue]

        val okTags: Set[ApplicableTagId] =
          liveTagFieldDistribution.inField(fieldId)

        lazy val otherwiseReqTypes =
          reqTypes.liveIds -- original.perReqType.keys

        def check(reqTypeId: Option[ReqTypeId], tagId: ApplicableTagId) = {
          val tag = tags.needApplicableTag(tagId)

          def addError(err: TagFieldIssue, rt: Option[ReqTypeId] = reqTypeId): Unit = {
            errors = errors.updated(rt, err)
            fixed = fixed.setOptional(rt)
          }

          if (!okTags.contains(tagId))
            addError(TagFieldIssue.DefaultTagUnrelated(tag))
          else if (tag.live is Dead)
            addError(TagFieldIssue.DefaultTagDead(tag))
          else {
            val reqTypeIds: Iterable[ReqTypeId] =
              if (reqTypeId.isDefined) reqTypeId else otherwiseReqTypes
            for (id <- reqTypeIds)
              if (tag.applicableReqTypes(id) is NotApplicable) {
                val rt = reqTypes.need(id)
                addError(TagFieldIssue.DefaultTagNotApplicable(tag, rt), Some(id))
              }
          }
        }

        original.perReqType.foreach {
          case ((reqTypeId, FieldReqTypeRules.Resolution.DefaultTo(tagId))) if reqTypes.need(reqTypeId).live is Live =>
            check(Some(reqTypeId), tagId)
          case _ =>
        }

        original.otherwise match {
          case FieldReqTypeRules.Resolution.DefaultTo(tagId) => check(None, tagId)
          case _ =>
        }

        FixedRules(original, fixed, errors)
      }
    }

  /** - Dead req types are ignored / unmodified (TODO What)
    * - Rules that default to unrelated tags are replaced with Optional
    * - Rules that default to non-applicable tags are replaced with Optional
    */
  val tagFieldRulesFixedShowDead: CustomField.Tag.Id => FixedRules[ApplicableTagId, TagFieldIssue] =
    Memo { fieldId =>
      import TagFieldIssue._
      val f = tagFieldRulesFixedHideDead(fieldId)
      val original = f.original
      var fixed    = original
      val errors   = f.errors.filter {
                       case (reqTypeId, _: DefaultTagUnrelated | _: DefaultTagNotApplicable) =>
                         fixed = fixed.setOptional(reqTypeId)
                         true
                       case (_, _: DefaultTagDead) =>
                         false
                     }
      FixedRules(original, fixed, errors)
    }

  val fieldRules: FilterDead => ReqTypeId => FieldSetRules =
    FilterDead.memo {

      case HideDead =>
        Memo { reqTypeId =>
          FieldSetRules(
            imp    = fields.custom(_).fieldReqTypeRules(reqTypeId),
            tag    = tagFieldRulesFixedHideDead(_).fixed(reqTypeId),
            text   = fields.custom(_).fieldReqTypeRules(reqTypeId),
            static = _.fieldReqTypeRules(reqTypeId),
          )
        }

      case ShowDead =>
        Memo { reqTypeId =>
          FieldSetRules(
            imp    = fields.custom(_).fieldReqTypeRules(reqTypeId),
            tag    = tagFieldRulesFixedShowDead(_).fixed(reqTypeId),
            text   = fields.custom(_).fieldReqTypeRules(reqTypeId),
            static = _.fieldReqTypeRules(reqTypeId),
          )
        }
    }

  def reqTypesWithRes[D](rules: FieldReqTypeRules[D])(res: FieldReqTypeRules.Resolution[D]): Iterator[ReqType] =
    reqTypes.all.iterator.filter(rt => rules(rt.reqTypeId) == res)

  val applicability: ProjectApplicability.Default = {
    val rulesForReqType = fieldRules(HideDead)
    ProjectApplicability {
      case f: CustomField.Implication.Id => rulesForReqType(_).imp(f).applicability
      case f: CustomField.Tag        .Id => rulesForReqType(_).tag(f).applicability
      case f: CustomField.Text       .Id => rulesForReqType(_).text(f).applicability
      case f: StaticField                => f.fieldReqTypeRules(_).applicability
    }
  }

  def fieldsForReqTypeIterator(reqTypeId: ReqTypeId, filterDead: FilterDead): Iterator[Field] = {
    val liveFilter = filterDead.filterFnBy((_: Field) live this)
    fields.fields.iterator.filter(f => liveFilter(f) && applicability(reqTypeId, f.fieldId).is(Applicable))
  }

  lazy val naReqTypesPerField: Multimap[FieldId, Set, ReqTypeId] = {
    // TODO Optimise naReqTypesPerFields
    // We could be smarter and iterate over fields without iterating over reqtypes by inspecting the rules directly
    var m = Multimap.empty[FieldId, Set, ReqTypeId]
    for {
      r <- reqTypes.all
      f <- fields.fields
    }
      if (applicability(r.reqTypeId, f.fieldId) is NotApplicable)
        m = m.add(f.fieldId, r.reqTypeId)
    m
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

  def tagFieldDistribution(filterDead: FilterDead): TagFieldDistribution.TagIds =
    filterDead match {
      case HideDead => liveTagFieldDistribution
      case ShowDead => deadTagFieldDistribution
    }

  /** "Live" refers to fields, not tags.
    * In other words, only live fields considered and then all tags, live & dead, are included.
    */
  lazy val liveTagFieldDistribution: TagFieldDistribution.TagIds =
    TagFieldDistribution(this, _.live(this) is Live)

  /** All fields considered. All tags, live & dead, included. */
  lazy val deadTagFieldDistribution: TagFieldDistribution.TagIds =
    TagFieldDistribution(this, _ => true)

  def deadTagFieldDistribution(deadTagFilter: CustomField.Tag.Id => Boolean): TagFieldDistribution.TagIds =
    TagFieldDistribution(this, f => f.live(this) match {
      case Live => true
      case Dead => deadTagFilter(f.id)
    })

  def deadTagFieldDistribution(deadTagFilter: Option[CustomField.Tag.Id => Boolean]): TagFieldDistribution.TagIds =
    deadTagFilter.fold(deadTagFieldDistribution)(deadTagFieldDistribution(_))

  def naTags(id: Option[ReqTypeId]): NaTags =
    id.fold(NaTags.none)(naTags)

  val naTags: ReqTypeId => NaTags =
    Memo(NaTags.forReqType(_, this))
}
