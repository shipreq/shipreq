package shipreq.webapp.client.project.feature.render

import japgolly.scalajs.react.Reusability
import scala.reflect.ClassTag
import scalaz.{-\/, \/-}
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.lib.DataReusability._

/**
 * ADT representing all types of fields supported by the editor.
 * Meant to be used as a key for some given content (e.g. for requirement FR-1).
 */
sealed trait FieldKey

object FieldKey {

  /** Fields apply to one or more type of reqs */
  sealed trait ForSomeReq extends FieldKey

  sealed trait ForGenericReq extends ForSomeReq
  sealed trait ForUseCase    extends ForSomeReq

  /** Fields apply to all types of reqs */
  sealed trait ForAllReqs extends ForGenericReq with ForUseCase

  case object Codes                                            extends ForAllReqs
  case object ReqType                                          extends ForAllReqs
  case object Title                                            extends ForAllReqs
  case object OtherTags                                        extends ForAllReqs
  case object AllTags                                          extends ForAllReqs
  final case class CustomTextField(field: CustomField.Text.Id) extends ForAllReqs
  final case class Implications   (scope: ImplicationScope)    extends ForAllReqs
  final case class CustomFieldTags(field: CustomField.Tag.Id)  extends ForAllReqs

  sealed trait ForCodeGroup extends FieldKey
  case object Code           extends ForCodeGroup
  case object CodeGroupTitle extends ForCodeGroup

  final case class UseCaseStep(id: UseCaseStepId) extends FieldKey

  final case class ManualIssue(id: ManualIssueId) extends FieldKey

  // ===================================================================================================================

  @inline implicit def equalityForSomeReq: UnivEq[ForSomeReq] =
    UnivEq.derive

  @inline implicit def equality: UnivEq[FieldKey] =
    UnivEq.derive

  implicit val reusability: Reusability[FieldKey] =
    Reusability.byUnivEq

  def customField(id: CustomFieldId): FieldKey =
    id match {
      case i: CustomField.Text.Id        => CustomTextField(i)
      case i: CustomField.Tag.Id         => CustomFieldTags(i)
      case i: CustomField.Implication.Id => Implications(-\/(i))
    }

  def impliedBy = Implications(\/-(Backwards))

//  def reqTextLoc(reqId: ReqId, loc: LocationOf.Text.InReq): FieldKey =
//    loc match {
//      case Location.Text.Title                    => Title
//      case Location.Text.CustomTextField(fieldId) => CustomTextField(fieldId)
//      case Location.Text.UseCaseStep(stepId)      => UseCaseStep(stepId)
//    }

  final class Type[F <: FieldKey](implicit ct: ClassTag[F]) {
    def widenFn[G >: F <: FieldKey, A](orig: F => A)(fallback: A): G => A =
      g => if (ct.runtimeClass.isInstance(g))
        orig(g.asInstanceOf[F])
      else
        fallback
  }
  implicit val typeGR : Type[ForGenericReq] = new Type
  implicit val typeCG : Type[ForCodeGroup]  = new Type
  implicit val typeMI : Type[ManualIssue]   = new Type
  implicit val typeUC : Type[ForUseCase]    = new Type
  implicit val typeUCS: Type[UseCaseStep]   = new Type
}
