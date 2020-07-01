package shipreq.webapp.base.data

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.utils.Memo
import monocle.Lens
import monocle.macros.{GenLens, Lenses}
import scalaz.Order
import shipreq.base.util.TaggedTypes._
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.DataImplicits._
import shipreq.webapp.base.data.ReqType.Mnemonic

/** type [[ReqTypeId]] = [[StaticReqType]] | [[CustomReqTypeId]] */
sealed trait ReqTypeId {
  def foldId[A](s: StaticReqType => A, c: CustomReqTypeId => A): A
}

sealed trait ReqType {
  def mnemonic    : Mnemonic
  def oldMnemonics: Set[Mnemonic]
  def name        : String
  def description : Option[String]
  def implication : Mandatory
  def live        : Live

  def fold[A](s: StaticReqType => A, c: CustomReqType => A): A

  final def reqTypeId: ReqTypeId =
    fold(s => s, _.id)

  final def allMnemonics: Set[Mnemonic] =
    oldMnemonics + mnemonic
}

object ReqType extends ReqTypeEquality {
  final case class Mnemonic(value: String) extends TaggedString

  def name(reqTypes: ReqTypes): ReqTypeId => String =
    _.foldId(_.name, reqTypes.need(_).name)
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
    override def description  = None
    override def implication  = Optional // TODO Should be configurable
  }

  val values: NonEmptyVector[StaticReqType] =
    AdtMacros.adtValues[StaticReqType]

  val requiringImplication: Set[StaticReqType] =
    values.iterator.filter(_.implication is Mandatory).toSet

  implicit val order = Util.univEqAndArbitraryOrder(values.whole)

  lazy val mnemonics =
    values.foldLeft(UnivEq.emptySet[Mnemonic])(_ ++ _.allMnemonics)
}

// =====================================================================================================================

final case class CustomReqTypeId(value: Int) extends TaggedInt with ReqTypeId {
  override def foldId[A](s: StaticReqType => A, c: CustomReqTypeId => A): A = c(this)
}

final case class CustomReqType(id          : CustomReqTypeId,
                               mnemonic    : Mnemonic,
                               oldMnemonics: Set[Mnemonic],
                               name        : String,
                               description : Option[String],
                               implication : Mandatory,
                               live        : Live) extends ReqType {

  def fullName = s"${mnemonic.value}: $name"

  override def fold[A](s: StaticReqType => A, c: CustomReqType => A): A = c(this)

  def setMnemonic(m2: Mnemonic, retain: Boolean): CustomReqType = {
    var old2 = oldMnemonics - m2
    if (retain)
      old2 += mnemonic
    copy(mnemonic = m2, oldMnemonics = old2)
  }
}

object CustomReqType {
  implicit def equality: UnivEq[CustomReqType] = UnivEq.derive

  object IdAccess extends ObjDataId[CustomReqType.type, CustomReqType, CustomReqTypeId] {
    override def id(d: CustomReqType) = d.id
    override val unapplyData: AnyRef => Option[CustomReqType] = {case r: CustomReqType => Some(r); case _ => None}
  }

  val name        : Lens[CustomReqType, String]         = GenLens[CustomReqType](_.name)
  val imp         : Lens[CustomReqType, Mandatory]      = GenLens[CustomReqType](_.implication)
  val live        : Lens[CustomReqType, Live]           = GenLens[CustomReqType](_.live)
  def oldMnemonics: Lens[CustomReqType, Set[Mnemonic]]  = GenLens[CustomReqType](_.oldMnemonics)

  val desc: Lens[CustomReqType, Option[String]] =
    Lens[CustomReqType, Option[String]](_.description)(d => _.copy(description = d.filter(_.nonEmpty)))

  val mnemonic: Boolean => Lens[CustomReqType, Mnemonic] =
    Memo.bool(retain => Lens((_: CustomReqType).mnemonic)(m => _.setMnemonic(m, retain)))

  @inline def v1(id          : CustomReqTypeId,
                 mnemonic    : Mnemonic,
                 oldMnemonics: Set[Mnemonic],
                 name        : String,
                 implication : Mandatory,
                 live        : Live): CustomReqType =
    apply(
      id           = id,
      mnemonic     = mnemonic,
      oldMnemonics = oldMnemonics,
      name         = name,
      description  = None,
      implication  = implication,
      live         = live,
    )
}

// =====================================================================================================================

@Lenses
final case class ReqTypes(custom: IMap[CustomReqTypeId, CustomReqType]) {

  def live(i: ReqTypeId, whenNotFound: Live): Live =
    get(i).fold(whenNotFound)(_.live)

  def get(i: ReqTypeId): Option[ReqType] =
    i.foldId[Option[ReqType]](Some(_), custom.get)

  def need(i: ReqTypeId): ReqType =
    i.foldId[ReqType](identity, custom.need)

  lazy val liveIds: Set[ReqTypeId] =
    (StaticReqType.values.iterator.filter(_.live is Live) ++
      custom.valuesIterator.filter(_.live is Live).map(_.reqTypeId)
      ).toSet

  lazy val liveCustomReqTypes: Vector[CustomReqType] =
    custom.valuesIterator.filter(_.live is Live).toVector

  lazy val all: NonEmptyVector[ReqType] =
    StaticReqType.values ++ custom.valuesIterator

  lazy val allSortedByMnemonic: NonEmptyVector[ReqType] =
    NonEmptyVector force all.whole.sortBy(_.mnemonic.value)

  lazy val liveSortedByMnemonic: NonEmptyVector[ReqType] =
    NonEmptyVector force allSortedByMnemonic.whole.filter(_.live is Live) // UC is always live. NEV.force is fine.

  lazy val idsRequiringImplication: Set[ReqTypeId] = {
    def customIds = custom.valuesIterator.filter(_.implication is Mandatory).map(_.id).toSet
    Util.mergeSets(StaticReqType.requiringImplication, customIds)
  }

  lazy val allByMnemonic: Map[ReqType.Mnemonic, ReqType] =
    all.iterator.flatMap(t => t.allMnemonics.iterator.map((_, t))).toMap

  lazy val order: Map[ReqTypeId, Int] =
    allSortedByMnemonic
      .iterator
      .map(_.reqTypeId)
      .iterator
      .zipWithIndex
      .toMap

  lazy val pubidOrdering: Ordering[Pubid] =
    new Ordering[Pubid] {
      val rto = order
      override def compare(x: Pubid, y: Pubid): Int =
        rto(x.reqTypeId) - rto(y.reqTypeId) match {
          case 0 => x.pos.value - y.pos.value
          case n => n
        }
    }

  def sortIdsByMnemonic(ids: IterableOnce[ReqTypeId]): Iterator[ReqType] =
    sortByMnemonic(ids.iterator.map(need))

  def sortByMnemonic(rts: IterableOnce[ReqType]): Iterator[ReqType] =
    MutableArray(rts).sortBy(_.mnemonic.value).iterator()

  def mkStringByIds(ids: IterableOnce[ReqTypeId], sep: String): String =
    mkString(ids.iterator.map(need), sep)

  def mkString(rts: IterableOnce[ReqType], sep: String): String =
    sortByMnemonic(rts).map(_.mnemonic.value).mkString(sep)
}

object ReqTypes {
  val empty: ReqTypes =
    ReqTypes(emptyDataMap(CustomReqType))

  implicit def univEq: UnivEq[ReqTypes] = UnivEq.derive

  type Custom = IMap[CustomReqTypeId, CustomReqType]
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
        case (_: StaticReqType  , _: CustomReqTypeId) => scalaz.Ordering.LT
        case (_: CustomReqTypeId, _: StaticReqType  ) => scalaz.Ordering.GT
      }
  }
}
