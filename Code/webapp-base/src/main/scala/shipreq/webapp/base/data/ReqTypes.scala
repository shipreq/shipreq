package shipreq.webapp.base.data

import monocle.macros.Lenses
import scalaz.{Order, Ordering}
import scalaz.std.anyVal.intInstance
import scalaz.syntax.order._
import shapeless.{Generic, :+:, CNil, Coproduct, Inl, Inr, Lazy}
import shipreq.base.util.{UnivEq, Must, NonEmptyVector}
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.TypeclassDerivation._
import ReqType.Mnemonic

/** type [[ReqTypeId]] = [[StaticReqType]] | [[CustomReqTypeId]] */
sealed trait ReqTypeId {
  def foldId[A](s: StaticReqType => A, c: CustomReqTypeId => A): A
}

object ReqTypeId {
  implicit object IdGeneric extends Generic[ReqTypeId] {
    override type Repr = StaticReqType :+: CustomReqTypeId :+: CNil
    override def to  (id: ReqTypeId): Repr = id.foldId(Coproduct[Repr](_), Coproduct[Repr](_))
    override def from(co: Repr): ReqTypeId = co match {
      case Inl(s)      => s
      case Inr(Inl(c)) => c
      case _           => ???
    }
  }

  implicit object IdOrder extends Order[ReqTypeId] with UnivEq[ReqTypeId] {
    implicitly[Lazy[UnivEq[ReqTypeId]]] // prove Id is actually UnivEq
    override def order(a: ReqTypeId, b: ReqTypeId) = (a, b) match {
      case (x: CustomReqTypeId, y: CustomReqTypeId) => Order[CustomReqTypeId].order(x, y)
      case (x: StaticReqType  , y: StaticReqType  ) => StaticReqType.order(x, y)
      case (x: StaticReqType  , y: CustomReqTypeId) => Ordering.LT
      case (x: CustomReqTypeId, y: StaticReqType  ) => Ordering.GT
    }
  }
}

sealed trait ReqType {
  def mnemonic: Mnemonic
  def oldMnemonics: Set[Mnemonic]
  def name: String
  def imp: ImplicationRequired

  def fold[A](s: StaticReqType => A, c: CustomReqType => A): A

  final def reqTypeId: ReqTypeId =
    fold(s => s, _.id)

  final def allMnemonics: Set[Mnemonic] =
    oldMnemonics + mnemonic
}

object ReqType {
  final case class Mnemonic(value: String) extends TaggedString

  val filterAlive: ReqType => Boolean =
    _.fold(_ => true, _.alive ≟ Alive)

  def name(customReqTypes: CustomReqTypeIMap): ReqTypeId => Must[String] =
    _.foldId(s => Must.Exists(s.name), c => customReqTypes(c).map(_.name))
}

// =====================================================================================================================

sealed trait StaticReqType extends ReqType with ReqTypeId {
  override final def fold  [A](s: StaticReqType => A, c: CustomReqType   => A): A = s(this)
  override final def foldId[A](s: StaticReqType => A, c: CustomReqTypeId => A): A = s(this)
}

object StaticReqType {

  case object UseCase extends StaticReqType {
    override def mnemonic     = Mnemonic("UC")
    override def oldMnemonics = UnivEq.emptySet
    override def name         = "Use Case"
    override def imp          = ImplicationRequired.Not
  }

  val values: NonEmptyVector[StaticReqType] =
    NonEmptyVector(UseCase)

  val valueStream: Stream[StaticReqType] =
    values.toStream

  implicit val order = UnivEq.withArbitraryOrder(values.whole)

  lazy val mnemonics =
    values.foldLeft(UnivEq.emptySet[Mnemonic])(_ ++ _.allMnemonics)
}

// =====================================================================================================================

final case class CustomReqTypeId(value: Long) extends TaggedLong with ReqTypeId {
  override def foldId[A](s: StaticReqType => A, c: CustomReqTypeId => A): A = c(this)
}

@Lenses
final case class CustomReqType(id          : CustomReqTypeId,
                               mnemonic    : Mnemonic,
                               oldMnemonics: Set[Mnemonic],
                               name        : String,
                               imp         : ImplicationRequired,
                               alive       : Alive) extends ReqType {

  def fullName = s"${mnemonic.value}: $name"

  override def fold[A](s: StaticReqType => A, c: CustomReqType => A): A = c(this)
}

object CustomReqType {
  implicit def equality: UnivEq[CustomReqType] = deriveUnivEq

  object IdAccess extends ObjDataId[CustomReqType.type, CustomReqType, CustomReqTypeId] {
    override def id(d: CustomReqType) = d.id
    override val unapplyData: AnyRef => Option[CustomReqType] = {case r: CustomReqType => Some(r); case _ => None}
  }
}
