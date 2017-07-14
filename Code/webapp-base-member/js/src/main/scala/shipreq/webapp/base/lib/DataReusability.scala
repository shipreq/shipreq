package shipreq.webapp.base.lib

import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react.extra._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.base.text.{Atom, PlainText, TextSearch}
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text.UseCaseStepFlowText.TextAndFlow
import shipreq.webapp.base.jsfacade.MomentJs

object DataReusability extends DataReusability

abstract class DataReusability extends BaseReusability {

  implicit def reusabilityMomentJs: Reusability[MomentJs] =
    Reusability.by(_.toEpochMilli)

  implicit def reusabilityObfuscated[A]: Reusability[Obfuscated[A]] =
    Reusability.caseClass

  implicit def reusabilityUsername: Reusability[Username] =
    Reusability.caseClass

  implicit lazy val reusabilityProjectMetaData: Reusability[ProjectMetaData] =
    Reusability.byRef || Reusability.caseClass

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
    Reusability.caseClass

  implicit def reusabilityReqCodeValue: Reusability[ReqCode.Value] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityReqTypeId: Reusability[ReqTypeId] =
    Reusability.byUnivEq

  implicit def reusabilityReqTypes: Reusability[ReqTypes] =
    Reusability.byRefOrUnivEq

  implicit def reusabilityPlainText: Reusability[PlainText.ForProject] =
    Reusability.byRef

  implicit def reusabilityTextSearch: Reusability[TextSearch] =
    Reusability.byRef

  implicit def reusabilityTagTree: Reusability[TagTree] =
    Reusability.byRef

  implicit def reusabilityCustomFields: Reusability[FieldSet.CustomFields] =
    Reusability.byRefOrEqual

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
    Reusability.caseClass
}

