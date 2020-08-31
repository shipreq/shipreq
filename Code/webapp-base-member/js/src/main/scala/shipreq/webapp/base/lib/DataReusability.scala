package shipreq.webapp.base.lib

import japgolly.scalajs.react._
import java.time.Duration
import scalaz.Equal
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.data.derivation._
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.event.{EventOrd, ProjectAndOrd}
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.filter.{CompiledFilter, Filter}
import shipreq.webapp.base.issue.{Issue, Issues}
import shipreq.webapp.base.jsfacade.MomentJs
import shipreq.webapp.base.protocol.websocket.SavedViewCmd
import shipreq.webapp.base.text.Atom.CodeBlockDetail
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text.UseCaseStepFlowText.TextAndFlow
import shipreq.webapp.base.text.{Atom, PlainText, ProjectText, TextSearch}
import shipreq.webapp.base.user._

object DataReusability extends DataReusability

abstract class DataReusability extends BaseReusability {

  implicit def reusabilityDerivativeTagsRules: Reusability[DerivativeTags.Rules] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityDerivativeTags: Reusability[DerivativeTags] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityCodeBlockDetail: Reusability[CodeBlockDetail] =
    Reusability.byRefOrUnivEq

  final def reusabilityByRefOrEqual[A <: AnyRef](implicit e: Equal[A]): Reusability[A] =
    Reusability.byRef || Reusability(e.equal)

  implicit def freeOption[A >: Null : Reusability]: Reusability[FreeOption[A]] =
    Reusability((x, y) =>
      x.fold(y.isEmpty, a => y.exists(a ~=~ _)))

  implicit def reusabilityErrorMsg: Reusability[ErrorMsg] =
    Reusability.by(_.value)

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

  implicit lazy val reusabilityProjectMetaData: Reusability[ProjectMetaData] = {
    @nowarn("cat=unused") implicit val instant = Reusability.instant(Duration.ofMillis(500))
    Reusability.byRef || Reusability.derive
  }

  implicit def reusabilityReactKey: Reusability[Key] =
    Reusability.by_==

  implicit def reusabilityNaTags: Reusability[NaTags] =
    Reusability.byRef || Reusability.by(_.set)

  implicit def reusabilityEventOrd: Reusability[EventOrd] =
    Reusability.byUnivEq

  implicit def reusabilityEventOrdLatest: Reusability[EventOrd.Latest] =
    Reusability.byUnivEq

  implicit def reusabilityProjectAndOrd: Reusability[ProjectAndOrd] =
    Reusability.byRef || Reusability.derive

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

  implicit def reusabilityReqType: Reusability[ReqType] =
    Reusability.byRefOrUnivEq

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

  implicit lazy val reusabilitySavedViewColumn: Reusability[savedview.Column] =
    Reusability.byUnivEq

  implicit lazy val reusabilitySavedViewSortCriterion: Reusability[savedview.SortCriterion] =
    Reusability.byRefOrUnivEq

  implicit lazy val reusabilitySavedViewSortCriteria: Reusability[savedview.SortCriteria] =
    Reusability.byRefOrUnivEq

  implicit lazy val reusabilitySavedViewView: Reusability[savedview.View] =
    Reusability.byRefOrUnivEq

  implicit lazy val reusabilitySavedViewNE: Reusability[savedview.SavedViews.NonEmpty] =
    Reusability.byRefOrUnivEq

  implicit lazy val reusabilitySavedViewId: Reusability[savedview.SavedView.Id] =
    Reusability.byUnivEq

  implicit lazy val reusabilitySavedViewName: Reusability[savedview.SavedView.Name] =
    Reusability.byUnivEq

  implicit lazy val reusabilitySavedView: Reusability[savedview.SavedView] =
    Reusability.byRef || Reusability.derive

  implicit lazy val reusabilitySavedViewCmdD: Reusability[SavedViewCmd.Delete] =
    Reusability.byUnivEq

  implicit lazy val reusabilityCompiledFilter: Reusability[CompiledFilter] =
    Reusability.byRef

  implicit lazy val reusabilityImpGraphConfigColours: Reusability[ImpGraphConfig.Colours] =
    Reusability.derive

  implicit lazy val reusabilityImpGraphConfigGraphDir: Reusability[ImpGraphConfig.GraphDir] =
    Reusability.derive

  implicit lazy val reusabilityImpGraphConfigLabelFormat: Reusability[ImpGraphConfig.LabelFormat] =
    Reusability.derive

  implicit lazy val reusabilityImpGraphConfig: Reusability[ImpGraphConfig] =
    Reusability.byRef || Reusability.derive
}
