package shipreq.webapp.member.project.event

import cats.{Eq, Order}
import japgolly.microlibs.nonempty.NonEmpty
import shipreq.base.util._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.filter.Filter.Implicits._
import shipreq.webapp.member.project.util.GenericData

object RetiredGenericData {

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object ApplicableTagGDv1 extends GenericData {
    sealed abstract class Attr extends AttrBase
    sealed abstract class Value extends ValueBase

    case object Children extends Attr {
      override type Data = TagInTree.Children
      override def apply(data: Data) = ValueForChildren(data)
      val dataEquality: Eq[Data] = implicitly[Eq[TagInTree.Children]]
    }
    final case class ValueForChildren(value: Children.Data) extends Value {
      override val attr: Children.type = Children
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForChildren => Children.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Desc extends Attr {
      override type Data = Option[String]
      override def apply(data: Data) = ValueForDesc(data)
      val dataEquality: Eq[Data] = implicitly[Eq[Option[String]]]
    }
    final case class ValueForDesc(value: Desc.Data) extends Value {
      override val attr: Desc.type = Desc
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForDesc => Desc.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Key extends Attr {
      override type Data = HashRefKey
      override def apply(data: Data) = ValueForKey(data)
      val dataEquality: Eq[Data] = implicitly[Eq[HashRefKey]]
    }
    final case class ValueForKey(value: Key.Data) extends Value {
      override val attr: Key.type = Key
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForKey => Key.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Name extends Attr {
      override type Data = String
      override def apply(data: Data) = ValueForName(data)
      val dataEquality: Eq[Data] = implicitly[Eq[String]]
    }
    final case class ValueForName(value: Name.Data) extends Value {
      override val attr: Name.type = Name
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForName => Name.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Parents extends Attr {
      override type Data = TagInTree.Parents
      override def apply(data: Data) = ValueForParents(data)
      val dataEquality: Eq[Data] = implicitly[Eq[TagInTree.Parents]]
    }
    final case class ValueForParents(value: Parents.Data) extends Value {
      override val attr: Parents.type = Parents
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForParents => Parents.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
      Util.univEqAndArbitraryOrder(Vector(Children, Desc, Key, Name, Parents))

    @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

    override val attrs = NonEmptySet[Attr](Children, Desc, Key, Name, Parents)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object CustomImpFieldGDv1 extends GenericData {
    sealed abstract class Attr extends AttrBase
    sealed abstract class Value extends ValueBase

    case object ApplicableReqTypes extends Attr {
      override type Data = ApplicableReqTypes
      override def apply(data: Data) = ValueForApplicableReqTypes(data)
      val dataEquality: Eq[Data] = implicitly[Eq[ApplicableReqTypes]]
    }
    final case class ValueForApplicableReqTypes(value: ApplicableReqTypes.Data) extends Value {
      override val attr: ApplicableReqTypes.type = ApplicableReqTypes
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForApplicableReqTypes => ApplicableReqTypes.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Mandatory extends Attr {
      override type Data = Mandatory
      override def apply(data: Data) = ValueForMandatory(data)
      val dataEquality: Eq[Data] = implicitly[Eq[Mandatory]]
    }
    final case class ValueForMandatory(value: Mandatory.Data) extends Value {
      override val attr: Mandatory.type = Mandatory
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForMandatory => Mandatory.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object ReqTypeId extends Attr {
      override type Data = ReqTypeId
      override def apply(data: Data) = ValueForReqTypeId(data)
      val dataEquality: Eq[Data] = implicitly[Eq[ReqTypeId]]
    }
    final case class ValueForReqTypeId(value: ReqTypeId.Data) extends Value {
      override val attr: ReqTypeId.type = ReqTypeId
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForReqTypeId => ReqTypeId.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
      Util.univEqAndArbitraryOrder(Vector(ApplicableReqTypes, Mandatory, ReqTypeId))

    @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

    override val attrs = NonEmptySet[Attr](ApplicableReqTypes, Mandatory, ReqTypeId)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object CustomReqTypeGDv1 extends GenericData {
    sealed abstract class Attr extends AttrBase
    sealed abstract class Value extends ValueBase

    case object Implication extends Attr {
      override type Data = Mandatory
      override def apply(data: Data) = ValueForImplication(data)
      override val dataEquality: Eq[Data] = implicitly[Eq[Mandatory]]
    }
    final case class ValueForImplication(value: Implication.Data) extends Value {
      override val attr: Implication.type = Implication
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForImplication => Implication.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Mnemonic extends Attr {
      override type Data = ReqType.Mnemonic
      override def apply(data: Data) = ValueForMnemonic(data)
      override val dataEquality: Eq[Data] = implicitly[Eq[ReqType.Mnemonic]]
    }
    final case class ValueForMnemonic(value: Mnemonic.Data) extends Value {
      override val attr: Mnemonic.type = Mnemonic
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForMnemonic => Mnemonic.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Name extends Attr {
      override type Data = String
      override def apply(data: Data) = ValueForName(data)
      override val dataEquality: Eq[Data] = implicitly[Eq[String]]
    }
    final case class ValueForName(value: Name.Data) extends Value {
      override val attr: Name.type = Name
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForName => Name.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
      Util.univEqAndArbitraryOrder(Vector(Implication, Mnemonic, Name))

    @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

    override val attrs = NonEmptySet[Attr](Implication, Mnemonic, Name)

    def apply(mnemonic: ReqType.Mnemonic, name: String, implication: Mandatory): NonEmptyValues =
      NonEmpty.force(emptyValues + ValueForMnemonic(mnemonic) + ValueForName(name) + ValueForImplication(implication))
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object CustomTagFieldGDv1 extends GenericData {
    sealed abstract class Attr extends AttrBase
    sealed abstract class Value extends ValueBase

    case object ApplicableReqTypes extends Attr {
      override type Data = ApplicableReqTypes
      override def apply(data: Data) = ValueForApplicableReqTypes(data)
      val dataEquality: Eq[Data] = implicitly[Eq[ApplicableReqTypes]]
    }
    final case class ValueForApplicableReqTypes(value: ApplicableReqTypes.Data) extends Value {
      override val attr: ApplicableReqTypes.type = ApplicableReqTypes
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForApplicableReqTypes => ApplicableReqTypes.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Mandatory extends Attr {
      override type Data = Mandatory
      override def apply(data: Data) = ValueForMandatory(data)
      val dataEquality: Eq[Data] = implicitly[Eq[Mandatory]]
    }
    final case class ValueForMandatory(value: Mandatory.Data) extends Value {
      override val attr: Mandatory.type = Mandatory
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForMandatory => Mandatory.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object TagId extends Attr {
      override type Data = TagId
      override def apply(data: Data) = ValueForTagId(data)
      val dataEquality: Eq[Data] = implicitly[Eq[TagId]]
    }
    final case class ValueForTagId(value: TagId.Data) extends Value {
      override val attr: TagId.type = TagId
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForTagId => TagId.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
      Util.univEqAndArbitraryOrder(Vector(ApplicableReqTypes, Mandatory, TagId))

    @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

    override val attrs = NonEmptySet[Attr](ApplicableReqTypes, Mandatory, TagId)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object CustomTextFieldGDv1 extends GenericData {
    sealed abstract class Attr extends AttrBase
    sealed abstract class Value extends ValueBase

    case object ApplicableReqTypes extends Attr {
      override type Data = ApplicableReqTypes
      override def apply(data: Data) = ValueForApplicableReqTypes(data)
      val dataEquality: Eq[Data] = implicitly[Eq[ApplicableReqTypes]]
    }
    final case class ValueForApplicableReqTypes(value: ApplicableReqTypes.Data) extends Value {
      override val attr: ApplicableReqTypes.type = ApplicableReqTypes
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForApplicableReqTypes => ApplicableReqTypes.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Key extends Attr {
      override type Data = String
      override def apply(data: Data) = ValueForKey(data)
      val dataEquality: Eq[Data] = implicitly[Eq[String]]
    }
    final case class ValueForKey(value: Key.Data) extends Value {
      override val attr: Key.type = Key
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForKey => Key.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Mandatory extends Attr {
      override type Data = Mandatory
      override def apply(data: Data) = ValueForMandatory(data)
      val dataEquality: Eq[Data] = implicitly[Eq[Mandatory]]
    }
    final case class ValueForMandatory(value: Mandatory.Data) extends Value {
      override val attr: Mandatory.type = Mandatory
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForMandatory => Mandatory.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Name extends Attr {
      override type Data = String
      override def apply(data: Data) = ValueForName(data)
      val dataEquality: Eq[Data] = implicitly[Eq[String]]
    }
    final case class ValueForName(value: Name.Data) extends Value {
      override val attr: Name.type = Name
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForName => Name.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
      Util.univEqAndArbitraryOrder(Vector(ApplicableReqTypes, Key, Mandatory, Name))

    @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

    override val attrs = NonEmptySet[Attr](ApplicableReqTypes, Key, Mandatory, Name)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object SavedViewGDv1 extends GenericData {
    import shipreq.webapp.member.project.filter.Filter.{Valid => ValidFilter}
    import shipreq.webapp.member.project.data.savedview._

    sealed abstract class Attr extends AttrBase
    sealed abstract class Value extends ValueBase

    case object Columns extends Attr {
      override type Data = NonEmptyVector[Column]
      override def apply(data: Data) = ValueForColumns(data)
      override val dataEquality: Eq[Data] = implicitly[Eq[NonEmptyVector[Column]]]
    }
    final case class ValueForColumns(value: Columns.Data) extends Value {
      override val attr: Columns.type = Columns
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForColumns => Columns.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Filter extends Attr {
      override type Data = Option[ValidFilter]
      override def apply(data: Data) = ValueForFilter(data)
      override val dataEquality: Eq[Data] = implicitly[Eq[Option[ValidFilter]]]
    }
    final case class ValueForFilter(value: Filter.Data) extends Value {
      override val attr: Filter.type = Filter
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForFilter => Filter.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object FilterDead extends Attr {
      override type Data = FilterDead
      override def apply(data: Data) = ValueForFilterDead(data)
      override val dataEquality: Eq[Data] = implicitly[Eq[FilterDead]]
    }
    final case class ValueForFilterDead(value: FilterDead.Data) extends Value {
      override val attr: FilterDead.type = FilterDead
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForFilterDead => FilterDead.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Name extends Attr {
      override type Data = SavedView.Name
      override def apply(data: Data) = ValueForName(data)
      override val dataEquality: Eq[Data] = implicitly[Eq[SavedView.Name]]
    }
    final case class ValueForName(value: Name.Data) extends Value {
      override val attr: Name.type = Name
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForName => Name.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    case object Order extends Attr {
      override type Data = SortCriteria
      override def apply(data: Data) = ValueForOrder(data)
      override val dataEquality: Eq[Data] = implicitly[Eq[SortCriteria]]
    }
    final case class ValueForOrder(value: Order.Data) extends Value {
      override val attr: Order.type = Order
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForOrder => Order.dataEquality.eqv(value, v2.value)
        case _ => false
      }
    }

    override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
      Util.univEqAndArbitraryOrder(Vector(Columns, Filter, FilterDead, Name, Order))

    @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

    override val attrs = NonEmptySet[Attr](Columns, Filter, FilterDead, Name, Order)

    def apply(name: SavedView.Name, filterDead: FilterDead, columns: NonEmptyVector[Column], order: SortCriteria, filter: Option[ValidFilter]): NonEmptyValues =
      NonEmpty.force(emptyValues + ValueForName(name) + ValueForFilterDead(filterDead) + ValueForColumns(columns) + ValueForOrder(order) + ValueForFilter(filter))
  }

}
