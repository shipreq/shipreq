package shipreq.webapp.base.lib

import japgolly.scalajs.react._
import scalaz.Equal
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.issue.{Issue, Issues}
import shipreq.webapp.base.user._
import shipreq.webapp.base.text.{Atom, PlainText, ProjectText, TextSearch}
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text.UseCaseStepFlowText.TextAndFlow
import shipreq.webapp.base.jsfacade.MomentJs

object DataReusability extends DataReusability

abstract class DataReusability extends BaseReusability {

  final def reusabilityByRefOrEqual[A <: AnyRef](implicit e: Equal[A]): Reusability[A] =
    Reusability.byRef || Reusability(e.equal)

  implicit def freeOption[A >: Null : Reusability]: Reusability[FreeOption[A]] =
    Reusability((x, y) =>
      x.fold(y.isEmpty, a => y.exists(a ~=~ _)))

  implicit def reusabilityMomentJs: Reusability[MomentJs] =
    Reusability.by(_.toEpochMilli)

  implicit def reusabilityObfuscated[A]: Reusability[Obfuscated[A]] =
    Reusability.derive

  implicit def reusabilityLiveDeadStat[A: UnivEq]: Reusability[LiveDeadStat[A]] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityLiveDeadStatMap[K: UnivEq, V: UnivEq]: Reusability[LiveDeadStatMap[K, V]] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityImpossible: Reusability[Impossible] =
    Reusability.always

  implicit def reusabilityUsername: Reusability[Username] =
    Reusability.derive

  implicit def reusabilityColour: Reusability[Colour] =
    Reusability.byUnivEq

  implicit def reusabilityFieldReqTypeRulesResolution[D: UnivEq]: Reusability[FieldReqTypeRules.Resolution[D]] =
    Reusability.byUnivEq

  implicit def reusabilityFieldReqTypeRulesByResolution[D: UnivEq]: Reusability[FieldReqTypeRules.ByResolution[D]] =
    Reusability.byRefOrUnivEq

  implicit lazy val reusabilityProjectMetaData: Reusability[ProjectMetaData] =
    Reusability.byRef || Reusability.derive

  implicit def reusabilityReactKey: Reusability[Key] =
    Reusability.by_==

  implicit def reusabilityNaTags: Reusability[NaTags] =
    Reusability.byRef || Reusability.by(_.set)

  implicit def reusabilityProject: Reusability[Project] =
    Reusability.byRef

  implicit def reusabilityProjectConfig: Reusability[ProjectConfig] =
    Reusability.byRef

  implicit def reusabilityCustomIssueTypeIMap: Reusability[CustomIssueTypeIMap] =
    Reusability.byRef

  implicit def reusabilityRequirements: Reusability[Requirements] =
    Reusability.byRef

  implicit def reusabilityUseCases: Reusability[UseCases] =
    Reusability.byRef

  implicit def reusabilityReqCodeTrie: Reusability[ReqCode.Trie] =
    Reusability.byRef

  implicit def reusabilityReqCodes: Reusability[ReqCodes] =
    Reusability.derive

  implicit def reusabilityReqCodeValue: Reusability[ReqCode.Value] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityReqTypeId: Reusability[ReqTypeId] =
    Reusability.byUnivEq

  implicit def reusabilityCustomIssueType: Reusability[CustomIssueType] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityCustomReqType: Reusability[CustomReqType] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityStaticReqType: Reusability[StaticReqType] =
    Reusability.byUnivEq

  implicit def reusabilityReqTypes: Reusability[ReqTypes] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityProjectTextCtx: Reusability[ProjectText.Context] =
    Reusability.byUnivEq

  implicit def reusabilityPlainText[C <: ProjectText.Context]: Reusability[PlainText.ForProject[C]] =
    Reusability.byRef

  implicit def reusabilityTextSearch: Reusability[TextSearch] =
    Reusability.byRef

  implicit def reusabilityTagTree: Reusability[TagTree] =
    Reusability.byRef

  implicit def reusabilityTagInTreeRelations: Reusability[TagInTree.Relations] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityTags: Reusability[Tags] =
    Reusability.byRef

  implicit def reusabilityHashRefKey: Reusability[HashRefKey] =
    Reusability.byUnivEq

  implicit def reusabilityApplicableReqTypes: Reusability[ApplicableReqTypes] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityApplicableTag: Reusability[ApplicableTag] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityTagGroup: Reusability[TagGroup] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityFieldId: Reusability[FieldId] =
    Reusability.byUnivEq

  implicit def reusabilityStaticField: Reusability[StaticField] =
    Reusability.byUnivEq

  implicit def reusabilityStaticFieldMandatory: Reusability[StaticField.Mandatory] =
    Reusability.byUnivEq

  implicit def reusabilityStaticFieldOptional: Reusability[StaticField.Optional] =
    Reusability.byUnivEq

  implicit def reusabilityCustomFields: Reusability[FieldSet.CustomFields] =
    reusabilityByRefOrEqual

  implicit def reusabilityFilterValid: Reusability[Filter.Valid] =
    Reusability.byUnivEq

  implicit def reusabilityExternalPubid: Reusability[ExternalPubid] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityOptionalText[A <: Atom.AnyAtom]: Reusability[Vector[A]] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityVectorTreeLoc: Reusability[VectorTree.Location] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityVectorTreePLoc: Reusability[VectorTree.PartialLocation] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityUseCaseStep: Reusability[UseCaseStep] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityUseCaseStepField: Reusability[StaticField.UseCaseStepTree] =
    Reusability.byUnivEq

  implicit def reusabilityTextAndFlow[T: Reusability, S: Reusability]: Reusability[TextAndFlow[T, S]] =
    Reusability.derive

  implicit lazy val reusabilityIssue: Reusability[Issue] =
    Reusability.byRef

  implicit lazy val reusabilityIssues: Reusability[Issues] =
    Reusability.byRef || Reusability.derive
}

