package shipreq.webapp.base.test

import japgolly.microlibs.nonempty.{NonEmpty, NonEmptySet}
import shipreq.base.util._
import shipreq.webapp.base.data
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.text.Text

object TestEvent {
  import Event._

  private val optionalString: String => Option[Option[String]] = {
    case null => None
    case ""   => Some(None)
    case s    => Some(Some(s))
  }

  private def mapOptionalString[A](s: String, f: String => A): Option[Option[A]] =
    optionalString(s).map(_.map(f))

  def tagGroupCreate(id         : TagGroupId,
                     name       : String        = null,
                     desc       : String        = null,
                     exclusivity: Exclusivity   = NonExclusive,
                     children   : Vector[TagId] = Vector.empty,
                     parent     : TagId         = null,
                     parents    : Vector[TagId] = Vector.empty,
                    ): TagGroupCreate = {
    import TagGroupGD._
    TagGroupCreate(id, nev(
      ValueForName(Option(name).getOrElse("TG#" + id.value)),
      ValueForDesc(Option(desc)),
      ValueForExclusivity(exclusivity),
      ValueForChildren(children),
      ValueForParents(parents.iterator.++(Option(parent)).map((_, Option.empty[TagId])).toMap),
    ))
  }

  def applicableTagCreate(id                : ApplicableTagId,
                          key               : String,
                          applicableReqTypes: ApplicableReqTypes = ApplicableReqTypes.empty,
                          colour            : String             = null,
                          desc              : String             = null,
                          children          : Vector[TagId]      = Vector.empty,
                          parent            : TagId              = null,
                          parents           : Vector[TagId]      = Vector.empty,
                    ): ApplicableTagCreate = {
    import ApplicableTagGD._
    ApplicableTagCreate(id, nev(
      ValueForApplicableReqTypes(applicableReqTypes),
      ValueForColour(Option(colour).map(data.Colour.force)),
      ValueForDesc(Option(desc)),
      ValueForKey(HashRefKey(key)),
      ValueForChildren(children),
      ValueForParents(parents.iterator.++(Option(parent)).map((_, Option.empty[TagId])).toMap),
    ))
  }

  def applicableTagUpdate(id                : ApplicableTagId,
                          key               : String             = null,
                          applicableReqTypes: ApplicableReqTypes = null,
                          colour            : String             = null,
                          desc              : String             = null,
                          children          : Vector[TagId]      = null,
                          parent            : TagId              = null,
                          parents           : Vector[TagId]      = null,
                    ): ApplicableTagUpdate = {
    import ApplicableTagGD._
    var vs = emptyValues
    Option(key).map(HashRefKey).foreach(vs += ValueForKey(_))
    Option(applicableReqTypes).map(vs += ValueForApplicableReqTypes(_))
    mapOptionalString(colour, data.Colour.force).foreach(vs += ValueForColour(_))
    optionalString(desc).foreach(vs += ValueForDesc(_))
    Option(children).foreach(vs += ValueForChildren(_))
    if ((parent ne null) || (parents ne null)) {
      val p = Option(parents).iterator.flatten.++(Option(parent)).map((_, Option.empty[TagId])).toMap
      vs += ValueForParents(p)
    }
    ApplicableTagUpdate(id, NonEmpty.force(vs))
  }

  def genericReqCreate(id     : GenericReqId,
                       rt     : CustomReqTypeId,
                       title  : String                        = null,
                       tags   : IterableOnce[ApplicableTagId] = Nil,
                       impSrcs: IterableOnce[ReqId]           = Nil,
                       impTgts: IterableOnce[ReqId]           = Nil,
                      ): GenericReqCreate = {
    import GenericReqGD._
    var vs = emptyValues
    Option(title).foreach(t => vs += ValueForTitle(NonEmptyArraySeq(Text.GenericReqTitle.Literal(t))))
    NonEmptySet.option(tags.iterator.toSet).foreach(vs += ValueForTags(_))
    NonEmptySet.option(impSrcs.iterator.toSet).foreach(vs += ValueForImpSrcs(_))
    NonEmptySet.option(impTgts.iterator.toSet).foreach(vs += ValueForImpTgts(_))
    GenericReqCreate(id, rt, vs)
  }

  def fieldCustomTagCreate(id   : CustomField.Tag.Id,
                           tagId: TagGroupId,
                           rules: FieldReqTypeRules.ForTagField = FieldReqTypeRules.empty,
                           deriv: DerivativeTags                = DerivativeTags.emptyDisabled,
                          ): FieldCustomTagCreate = {
    import CustomTagFieldGD._
    FieldCustomTagCreate(id, tagId, nev(
      ValueForFieldReqTypeRules(rules),
      ValueForDerivativeTags(deriv),
    ))
  }

  def reqTagsPatch(id    : ReqId,
                   add   : IterableOnce[ApplicableTagId] = Nil,
                   remove: IterableOnce[ApplicableTagId] = Nil,
                  ): ReqTagsPatch = {
    val sd = SetDiff(removed = remove.iterator.toSet, add.iterator.toSet)
    val ne = NonEmpty(sd) getOrElse sys.error(s"reqTagsPatch called with no data.")
    ReqTagsPatch(id, ne)
  }

  def reqsDelete(id        : ReqId                        = null,
                 ids       : IterableOnce[ReqId]          = Nil,
                 codeGroups: IterableOnce[ReqCodeGroupId] = Nil,
                 reason    : String                       = null,
                ): ReqsDelete =
    ReqsDelete(
      reqs       = NonEmptySet.force(ids.iterator.toSet ++ Option(id)),
      codeGroups = codeGroups.iterator.toSet,
      reason     = Option(reason).map(Text.DeletionReason.Literal(_)).to(ArraySeq),
    )
}
