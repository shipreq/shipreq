package shipreq.webapp.base.data

import monocle.macros.Lenser
import scalaz.{NonEmptyList, Order, Ordering}
import scalaz.std.anyVal.intInstance
import scalaz.syntax.order._
import shapeless.contrib.scalaz.Instances._
import shipreq.base.util.TaggedTypes._
import ReqType.Mnemonic

sealed trait ReqType {
  def mnemonic: Mnemonic
  def oldMnemonics: Set[Mnemonic]
  def name: String
  def imp: ImplicationRequired

  def fold[A](s: StaticReqType => A, c: CustomReqType => A): A

  final def reqTypeId: ReqType.Id =
    fold(s => s, _.id)

  final def allMnemonics: Set[Mnemonic] =
    oldMnemonics + mnemonic
}

object ReqType {
  final case class Mnemonic(value: String) extends TaggedString

  // type Id = StaticReqType \/ CustomReqType.Id
  sealed trait Id {
    def foldId[A](s: StaticReqType => A, c: CustomReqType.Id => A): A
  }

  implicit object IdOrder extends Order[ReqType.Id] {
    override def order(a: ReqType.Id, b: ReqType.Id) = (a, b) match {
      case (x: CustomReqType.Id, y: CustomReqType.Id) => Order[CustomReqType.Id].order(x, y)
      case (x: StaticReqType   , y: StaticReqType   ) => StaticReqType.order(x, y)
      case (x: StaticReqType   , y: CustomReqType.Id) => Ordering.LT
      case (x: CustomReqType.Id, y: StaticReqType   ) => Ordering.GT
    }
    override val equalIsNatural = StaticReqType.order.equalIsNatural && Order[CustomReqType.Id].equalIsNatural
  }

  val filterAlive: ReqType => Boolean =
    _.fold(_ => true, _.alive ≟ Alive)
}

sealed trait StaticReqType extends ReqType with ReqType.Id {
  override final def fold  [A](s: StaticReqType => A, c: CustomReqType    => A): A = s(this)
  override final def foldId[A](s: StaticReqType => A, c: CustomReqType.Id => A): A = s(this)
}

object StaticReqType {

  case object UseCase extends StaticReqType {
    override def mnemonic     = Mnemonic("UC")
    override def oldMnemonics = Set.empty
    override def name         = "Use Case"
    override def imp          = ImplicationRequired.Not
  }

  val values: NonEmptyList[StaticReqType] =
    NonEmptyList(UseCase)

  implicit object order extends Order[StaticReqType] {
    private[this] val fixedOrder = values.list.zipWithIndex.toMap
    @inline private[this] def int(s: StaticReqType) = fixedOrder(s)
    override def order(a: StaticReqType, b: StaticReqType) = Order[Int].order(int(a), int(b))
    override def equalIsNatural = true
  }

  lazy val mnemonics =
    (Set.empty[Mnemonic] /: values.list)(_ ++ _.allMnemonics)
}


// =====================================================================================================================

final case class CustomReqType(id: CustomReqType.Id,
                               mnemonic: Mnemonic,
                               oldMnemonics: Set[Mnemonic],
                               name: String,
                               imp: ImplicationRequired,
                               alive: Alive) extends ReqType {

  def fullName = s"${mnemonic.value}: $name"

  override def fold[A](s: StaticReqType => A, c: CustomReqType => A): A = c(this)
}

object CustomReqType {
  final case class Id(value: Long) extends TaggedLong with ReqType.Id {
    override def foldId[A](s: StaticReqType => A, c: Id => A): A = c(this)
  }

  object IdAccess extends ObjDataIdM[CustomReqType.type, CustomReqType, Id] {
    override def id(d: CustomReqType) = d.id
    override def mkId(l: Long) = Id(l)
    override def setId(a: CustomReqType, b: Id) = a.copy(id = b)
  }

  private[this] def l = Lenser[CustomReqType]
  val _name         = l(_.name)
  val _mnemonic     = l(_.mnemonic)
  val _oldMnemonics = l(_.oldMnemonics)
}
