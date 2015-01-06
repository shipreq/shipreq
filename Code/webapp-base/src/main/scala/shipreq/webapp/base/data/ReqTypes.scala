package shipreq.webapp.base.data

import monocle.macros.Lenser
import scalaz.{Order, Ordering}
import scalaz.std.anyVal.intInstance
import scalaz.syntax.order._
import shapeless.contrib.scalaz.Instances._
import shipreq.base.util.TaggedTypes._

sealed trait ReqType {
  def mnemonic: ReqType.Mnemonic
  def oldMnemonics: Set[ReqType.Mnemonic]
  def name: String
  def imp: ImplicationRequired

  final def allMnemonics: Set[ReqType.Mnemonic] = oldMnemonics + mnemonic
}

object ReqType {
  final case class Mnemonic(value: String) extends TaggedString

  // type Id = Static \/ CustomReqType.Id
  sealed trait Id

  sealed trait Static extends ReqType with Id

  case object UseCase extends Static {
    override def mnemonic     = Mnemonic("UC")
    override def oldMnemonics = Set.empty
    override def name         = "Use Case"
    override def imp          = ImplicationRequired.Not
  }

  val static: List[Static] = List(UseCase)

  implicit object StaticOrder extends Order[Static] {
    private[this] val fixedOrder = static.zipWithIndex.toMap
    @inline private[this] def int(s: Static) = fixedOrder(s)
    override def order(a: Static, b: Static) = Order[Int].order(int(a), int(b))
    override def equalIsNatural = true
  }

  implicit object IdOrder extends Order[Id] {
//    override def order(a: Id, b: Id) = a match {
//      case x: CustomReqType.Id => b match {case y: CustomReqType.Id => x ?|? y; case _ => Ordering.GT}
//      case x: Static           => b match {case y: Static           => x ?|? y; case _ => Ordering.LT}
//    }
    override def order(a: Id, b: Id) = (a, b) match {
      case (x: CustomReqType.Id, y: CustomReqType.Id) => Order[CustomReqType.Id].order(x, y)
      case (x: Static          , y: Static          ) => StaticOrder(x, y)
      case (x: Static          , y: CustomReqType.Id) => Ordering.LT
      case (x: CustomReqType.Id, y: Static          ) => Ordering.GT
    }
    override val equalIsNatural = StaticOrder.equalIsNatural && Order[CustomReqType.Id].equalIsNatural
  }

  lazy val staticMnemonics =
    (Set.empty[Mnemonic] /: static)(_ ++ _.allMnemonics)
}

// =====================================================================================================================

final case class CustomReqType(id: CustomReqType.Id,
                               mnemonic: ReqType.Mnemonic,
                               oldMnemonics: Set[ReqType.Mnemonic],
                               name: String,
                               imp: ImplicationRequired,
                               alive: Alive) extends ReqType {
  def fullName = s"${mnemonic.value}: $name"
}

object CustomReqType {
  final case class Id(value: Long) extends TaggedLong with ReqType.Id

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
