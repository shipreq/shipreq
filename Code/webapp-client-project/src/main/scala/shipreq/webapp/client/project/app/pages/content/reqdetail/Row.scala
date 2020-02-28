package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react.Reusability
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.base.lib.KeyGen

sealed abstract class Row(_key: String) {
  /** A value that can be passed to React to quickly identify columns. */
  val key: String = _key
}

object Row {
  private def autoKey = KeyGen.global.next()

  sealed abstract class UseCaseSteps(_key: String) extends Row(_key) {
    def field     : StaticField.UseCaseStepTree
    def treeFilter: UseCaseSteps.Tree => Range
    def tailStep  : Boolean
  }

  case object UseCaseStepsN extends UseCaseSteps("n") {
    override def field      = StaticField.NormalAltStepTree
    override def treeFilter = StaticField.NormalAltStepTree.treeFilterN
    override def tailStep   = false
  }

  case object UseCaseStepsA extends UseCaseSteps("a") {
    override def field      = StaticField.NormalAltStepTree
    override def treeFilter = StaticField.NormalAltStepTree.treeFilterA
    override def tailStep   = true
  }

  case object UseCaseStepsE extends UseCaseSteps("e") {
    override def field      = StaticField.ExceptionStepTree
    override def treeFilter = StaticField.ExceptionStepTree.treeFilter
    override def tailStep   = true
  }

  case object Life                          extends Row(autoKey)
  case object PastPubids                    extends Row(autoKey)
  case object DeletionReason                extends Row(autoKey)
  case object ReqType                       extends Row(autoKey)
  case object Codes                         extends Row(autoKey)
  case object Tags                          extends Row(autoKey)
  case object Implications                  extends Row(autoKey)
  case object ImplicationGraph              extends Row(autoKey)
  case object StepGraph                     extends Row("f")
  case class CustomField(id: CustomFieldId) extends Row("f" + id.value)

  @inline implicit def univEqUseCaseSteps: UnivEq[UseCaseSteps] =
    UnivEq.derive

  implicit val reusabilityUseCaseSteps: Reusability[UseCaseSteps] =
    Reusability.byUnivEq

  @inline implicit def univEq: UnivEq[Row] =
    UnivEq.derive

  implicit val reusability: Reusability[Row] =
    Reusability.byUnivEq

  def head(fd: FilterDead): Vector[Row] =
    if (fd is ShowDead) headDead else headLive

  private def headLive: Vector[Row] =
    headDead.filter {
      case PastPubids | DeletionReason => false
      case _ => true
    }

  private def headDead: Vector[Row] =
    Vector(
      ReqType,
      Life,
      PastPubids,
      DeletionReason,
      Codes,
      Tags,
      Implications)

  val fromField: FieldId => List[Row] = {
    case f: CustomFieldId              => CustomField(f) :: Nil
    case StaticField.NormalAltStepTree => UseCaseStepsN :: UseCaseStepsA :: Nil
    case StaticField.ExceptionStepTree => UseCaseStepsE :: Nil
    case StaticField.StepGraph         => StepGraph :: Nil
    case StaticField.ImplicationGraph  => ImplicationGraph :: Nil
  }
}
