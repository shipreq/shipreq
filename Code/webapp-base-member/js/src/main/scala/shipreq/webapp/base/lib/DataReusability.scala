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

  implicit def reusabilityUsername: Reusability[Username] =
    Reusability.derive

  implicit lazy val reusabilityProjectMetaData: Reusability[ProjectMetaData] =
    Reusability.byRef || Reusability.derive

  implicit def reusabilityProject: Reusability[Project] =
    Reusability.byRef

  implicit def reusabilityProjectConfig: Reusability[ProjectConfig] =
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

  implicit def reusabilityTags: Reusability[Tags] =
    Reusability.derive

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

