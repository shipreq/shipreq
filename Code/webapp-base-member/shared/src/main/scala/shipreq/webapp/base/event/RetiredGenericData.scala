package shipreq.webapp.base.event

import japgolly.microlibs.nonempty._
import scalaz.{Equal, Order}
import shipreq.base.util._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.util._

object RetiredGenericData {

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object ApplicableTagGDv1 extends GenericData {
    sealed abstract class Attr extends AttrBase
    sealed abstract class Value extends ValueBase

    case object Children extends Attr {
      override type Data = TagInTree.Children
      override def apply(data: Data) = ValueForChildren(data)
      val dataEquality: Equal[Data] = implicitly[Equal[TagInTree.Children]]
    }
    final case class ValueForChildren(value: Children.Data) extends Value {
      override val attr: Children.type = Children
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForChildren => Children.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object Desc extends Attr {
      override type Data = Option[String]
      override def apply(data: Data) = ValueForDesc(data)
      val dataEquality: Equal[Data] = implicitly[Equal[Option[String]]]
    }
    final case class ValueForDesc(value: Desc.Data) extends Value {
      override val attr: Desc.type = Desc
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForDesc => Desc.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object Key extends Attr {
      override type Data = HashRefKey
      override def apply(data: Data) = ValueForKey(data)
      val dataEquality: Equal[Data] = implicitly[Equal[HashRefKey]]
    }
    final case class ValueForKey(value: Key.Data) extends Value {
      override val attr: Key.type = Key
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForKey => Key.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object Name extends Attr {
      override type Data = String
      override def apply(data: Data) = ValueForName(data)
      val dataEquality: Equal[Data] = implicitly[Equal[String]]
    }
    final case class ValueForName(value: Name.Data) extends Value {
      override val attr: Name.type = Name
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForName => Name.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object Parents extends Attr {
      override type Data = TagInTree.Parents
      override def apply(data: Data) = ValueForParents(data)
      val dataEquality: Equal[Data] = implicitly[Equal[TagInTree.Parents]]
    }
    final case class ValueForParents(value: Parents.Data) extends Value {
      override val attr: Parents.type = Parents
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForParents => Parents.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
      Util.univEqAndArbitraryOrder(Vector(Children, Desc, Key, Name, Parents))

    @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

    override val attrs = NonEmptySet[Attr](Children, Desc, Key, Name, Parents)
  }

  // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object CustomImpFieldGDv1 extends GenericData {
    sealed abstract class Attr extends AttrBase
    sealed abstract class Value extends ValueBase

    case object ApplicableReqTypes extends Attr {
      override type Data = ApplicableReqTypes
      override def apply(data: Data) = ValueForApplicableReqTypes(data)
      val dataEquality: Equal[Data] = implicitly[Equal[ApplicableReqTypes]]
    }
    final case class ValueForApplicableReqTypes(value: ApplicableReqTypes.Data) extends Value {
      override val attr: ApplicableReqTypes.type = ApplicableReqTypes
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForApplicableReqTypes => ApplicableReqTypes.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object Mandatory extends Attr {
      override type Data = Mandatory
      override def apply(data: Data) = ValueForMandatory(data)
      val dataEquality: Equal[Data] = implicitly[Equal[Mandatory]]
    }
    final case class ValueForMandatory(value: Mandatory.Data) extends Value {
      override val attr: Mandatory.type = Mandatory
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForMandatory => Mandatory.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object ReqTypeId extends Attr {
      override type Data = ReqTypeId
      override def apply(data: Data) = ValueForReqTypeId(data)
      val dataEquality: Equal[Data] = implicitly[Equal[ReqTypeId]]
    }
    final case class ValueForReqTypeId(value: ReqTypeId.Data) extends Value {
      override val attr: ReqTypeId.type = ReqTypeId
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForReqTypeId => ReqTypeId.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
      Util.univEqAndArbitraryOrder(Vector(ApplicableReqTypes, Mandatory, ReqTypeId))

    @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

    override val attrs = NonEmptySet[Attr](ApplicableReqTypes, Mandatory, ReqTypeId)
  }

  // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object CustomTagFieldGDv1 extends GenericData {
    sealed abstract class Attr extends AttrBase
    sealed abstract class Value extends ValueBase

    case object ApplicableReqTypes extends Attr {
      override type Data = ApplicableReqTypes
      override def apply(data: Data) = ValueForApplicableReqTypes(data)
      val dataEquality: Equal[Data] = implicitly[Equal[ApplicableReqTypes]]
    }
    final case class ValueForApplicableReqTypes(value: ApplicableReqTypes.Data) extends Value {
      override val attr: ApplicableReqTypes.type = ApplicableReqTypes
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForApplicableReqTypes => ApplicableReqTypes.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object Mandatory extends Attr {
      override type Data = Mandatory
      override def apply(data: Data) = ValueForMandatory(data)
      val dataEquality: Equal[Data] = implicitly[Equal[Mandatory]]
    }
    final case class ValueForMandatory(value: Mandatory.Data) extends Value {
      override val attr: Mandatory.type = Mandatory
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForMandatory => Mandatory.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object TagId extends Attr {
      override type Data = TagId
      override def apply(data: Data) = ValueForTagId(data)
      val dataEquality: Equal[Data] = implicitly[Equal[TagId]]
    }
    final case class ValueForTagId(value: TagId.Data) extends Value {
      override val attr: TagId.type = TagId
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForTagId => TagId.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
      Util.univEqAndArbitraryOrder(Vector(ApplicableReqTypes, Mandatory, TagId))

    @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

    override val attrs = NonEmptySet[Attr](ApplicableReqTypes, Mandatory, TagId)
  }

  // █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

  object CustomTextFieldGDv1 extends GenericData {
    sealed abstract class Attr extends AttrBase
    sealed abstract class Value extends ValueBase

    case object ApplicableReqTypes extends Attr {
      override type Data = ApplicableReqTypes
      override def apply(data: Data) = ValueForApplicableReqTypes(data)
      val dataEquality: Equal[Data] = implicitly[Equal[ApplicableReqTypes]]
    }
    final case class ValueForApplicableReqTypes(value: ApplicableReqTypes.Data) extends Value {
      override val attr: ApplicableReqTypes.type = ApplicableReqTypes
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForApplicableReqTypes => ApplicableReqTypes.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object Key extends Attr {
      override type Data = String
      override def apply(data: Data) = ValueForKey(data)
      val dataEquality: Equal[Data] = implicitly[Equal[String]]
    }
    final case class ValueForKey(value: Key.Data) extends Value {
      override val attr: Key.type = Key
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForKey => Key.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object Mandatory extends Attr {
      override type Data = Mandatory
      override def apply(data: Data) = ValueForMandatory(data)
      val dataEquality: Equal[Data] = implicitly[Equal[Mandatory]]
    }
    final case class ValueForMandatory(value: Mandatory.Data) extends Value {
      override val attr: Mandatory.type = Mandatory
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForMandatory => Mandatory.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    case object Name extends Attr {
      override type Data = String
      override def apply(data: Data) = ValueForName(data)
      val dataEquality: Equal[Data] = implicitly[Equal[String]]
    }
    final case class ValueForName(value: Name.Data) extends Value {
      override val attr: Name.type = Name
      override def equals(o: Any): Boolean = o match {
        case v2: ValueForName => Name.dataEquality.equal(value, v2.value)
        case _ => false
      }
    }

    override implicit val equalityAttr: Order[Attr] with UnivEq[Attr] =
      Util.univEqAndArbitraryOrder(Vector(ApplicableReqTypes, Key, Mandatory, Name))

    @inline override implicit def equalityValue: UnivEq[Value] = UnivEq.force

    override val attrs = NonEmptySet[Attr](ApplicableReqTypes, Key, Mandatory, Name)
  }

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

}
