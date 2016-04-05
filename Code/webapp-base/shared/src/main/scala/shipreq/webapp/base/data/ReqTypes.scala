
package shipreq.webapp.base.data

import monocle.macros.Lenses
import scalaz.{Order, Ordering}
import scalaz.std.anyVal.intInstance
import scalaz.syntax.order._
import shipreq.base.util.{NonEmptyVector, Util, UtilMacros}
import shipreq.base.util.TaggedTypes._
import shipreq.base.util.univeq._
import ReqType.Mnemonic

/** type [[ReqTypeId]] = [[StaticReqType]] | [[CustomReqTypeId]] */
sealed trait ReqTypeId {
  def foldId[A](s: StaticReqType => A, c: CustomReqTypeId => A): A
}

sealed trait ReqType {
  def mnemonic    : Mnemonic
  def oldMnemonics: Set[Mnemonic]
  def name        : String
  def imp         : ImplicationRequired
  def live        : Live

  def fold[A](s: StaticReqType => A, c: CustomReqType => A): A

  final def reqTypeId: ReqTypeId =
    fold(s => s, _.id)

  final def allMnemonics: Set[Mnemonic] =
    oldMnemonics + mnemonic
}

object ReqType extends ReqTypeEquality {
  final case class Mnemonic(value: String) extends TaggedString

  def name(customReqTypes: CustomReqTypeIMap): ReqTypeId => String =
    _.foldId(_.name, customReqTypes.need(_).name)
}

// =====================================================================================================================

sealed trait StaticReqType extends ReqType with ReqTypeId {
  override def live = Live
  override final def fold  [A](s: StaticReqType => A, c: CustomReqType   => A): A = s(this)
  override final def foldId[A](s: StaticReqType => A, c: CustomReqTypeId => A): A = s(this)
}

object StaticReqType {

  type UseCase = UseCase.type
  case object UseCase extends StaticReqType {
    override def mnemonic     = Mnemonic("UC")
    override def oldMnemonics = UnivEq.emptySet
    override def name         = "Use Case"
    override def imp          = ImplicationRequired.Not // TODO Should be configurable
  }

  val values: NonEmptyVector[StaticReqType] =
    UtilMacros.adtValues[StaticReqType]

  val valueStream: Stream[StaticReqType] =
    values.toStream

  implicit val order = Util.univEqAndArbitraryOrder(values.whole)

  lazy val mnemonics =
    values.foldLeft(UnivEq.emptySet[Mnemonic])(_ ++ _.allMnemonics)
}

// =====================================================================================================================

final case class CustomReqTypeId(value: Int) extends TaggedInt with ReqTypeId {
  override def foldId[A](s: StaticReqType => A, c: CustomReqTypeId => A): A = c(this)
}

@Lenses
final case class CustomReqType(id          : CustomReqTypeId,
                               mnemonic    : Mnemonic,
                               oldMnemonics: Set[Mnemonic],
                               name        : String,
                               imp         : ImplicationRequired,
                               live        : Live) extends ReqType {

  def fullName = s"${mnemonic.value}: $name"

  override def fold[A](s: StaticReqType => A, c: CustomReqType => A): A = c(this)

  def setMnemonic(nv: Mnemonic): CustomReqType =
    copy(mnemonic = nv, oldMnemonics = allMnemonics - nv)
}

object CustomReqType {
  implicit def equality: UnivEq[CustomReqType] = UnivEq.derive

  object IdAccess extends ObjDataId[CustomReqType.type, CustomReqType, CustomReqTypeId] {
    override def id(d: CustomReqType) = d.id
    override val unapplyData: AnyRef => Option[CustomReqType] = {case r: CustomReqType => Some(r); case _ => None}
  }
}

// =====================================================================================================================

sealed trait ReqTypeEquality {
  final implicit def univEq: UnivEq[ReqType] = UnivEq.derive
}

object ReqTypeId {
  implicit object IdOrder extends Order[ReqTypeId] with UnivEq[ReqTypeId] {
    UnivEq.derive[ReqTypeId] // prove Id is actually UnivEq
    override def order(a: ReqTypeId, b: ReqTypeId) = (a, b) match {
        case (x: CustomReqTypeId, y: CustomReqTypeId) => Order[CustomReqTypeId].order(x, y)
        case (x: StaticReqType  , y: StaticReqType  ) => StaticReqType.order(x, y)
        case (x: StaticReqType  , y: CustomReqTypeId) => Ordering.LT
        case (x: CustomReqTypeId, y: StaticReqType  ) => Ordering.GT
      }
  }
}
