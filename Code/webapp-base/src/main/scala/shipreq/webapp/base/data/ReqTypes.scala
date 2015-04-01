package shipreq.webapp.base.data

import monocle.macros.GenLens
import scalaz.{NonEmptyList, Order, Ordering}
import scalaz.std.anyVal.intInstance
import scalaz.syntax.order._
import shapeless.{Generic, :+:, CNil, Coproduct, Inl, Inr, Lazy}
import shipreq.base.util.{UnivEq, Must}
import shipreq.base.util.TaggedTypes._
import shipreq.webapp.base.TypeclassDerivation._
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

  /** type [[Id]] = [[StaticReqType]] | [[CustomReqType.Id]] */
  sealed trait Id {
    def foldId[A](s: StaticReqType => A, c: CustomReqType.Id => A): A
  }

  implicit object IdGeneric extends Generic[Id] {
    override type Repr = StaticReqType :+: CustomReqType.Id :+: CNil
    override def to  (id: Id): Repr = id.foldId(Coproduct[Repr](_), Coproduct[Repr](_))
    override def from(co: Repr): Id = co match {
      case Inl(s)      => s
      case Inr(Inl(c)) => c
      case _           => ???
    }
  }

  implicit object IdOrder extends Order[Id] with UnivEq[Id] {
    implicitly[Lazy[UnivEq[Id]]] // prove Id is actually UnivEq
    override def order(a: Id, b: Id) = (a, b) match {
      case (x: CustomReqType.Id, y: CustomReqType.Id) => Order[CustomReqType.Id].order(x, y)
      case (x: StaticReqType   , y: StaticReqType   ) => StaticReqType.order(x, y)
      case (x: StaticReqType   , y: CustomReqType.Id) => Ordering.LT
      case (x: CustomReqType.Id, y: StaticReqType   ) => Ordering.GT
    }
  }

  val filterAlive: ReqType => Boolean =
    _.fold(_ => true, _.alive ≟ Alive)

  def name(customReqTypes: CustomReqTypeIMap): Id => Must[String] =
    _.foldId(s => Must.Exists(s.name), c => customReqTypes(c).map(_.name))
}

sealed trait StaticReqType extends ReqType with ReqType.Id {
  override final def fold  [A](s: StaticReqType => A, c: CustomReqType    => A): A = s(this)
  override final def foldId[A](s: StaticReqType => A, c: CustomReqType.Id => A): A = s(this)
}

object StaticReqType {

  case object UseCase extends StaticReqType {
    override def mnemonic     = Mnemonic("UC")
    override def oldMnemonics = UnivEq.emptySet
    override def name         = "Use Case"
    override def imp          = ImplicationRequired.Not
  }

  val values: NonEmptyList[StaticReqType] =
    NonEmptyList(UseCase)

  val valueStream: Stream[StaticReqType] =
    values.list.toStream

  implicit val order = UnivEq.withArbitraryOrder(values.list)

  lazy val mnemonics =
    (UnivEq.emptySet[Mnemonic] /: values.list)(_ ++ _.allMnemonics)
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
    override val unapplyData: AnyRef => Option[CustomReqType] = {case r: CustomReqType => Some(r); case _ => None}
    override def mkId(l: Long) = Id(l)
    override def setId(a: CustomReqType, b: Id) = a.copy(id = b)
  }

  val name         = GenLens[CustomReqType](_.name)
  val mnemonic     = GenLens[CustomReqType](_.mnemonic)
  val oldMnemonics = GenLens[CustomReqType](_.oldMnemonics)
}
