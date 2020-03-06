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

  // ███████████████████████████████████████████████████████████████████████████████████████████████████████████████████

}
