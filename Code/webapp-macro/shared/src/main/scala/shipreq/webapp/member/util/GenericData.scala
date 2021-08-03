package shipreq.webapp.member.project.util

import japgolly.microlibs.nonempty.NonEmpty
import scala.collection.IterableOnce
import cats.{Eq, Order}
import shipreq.base.util.IMap

abstract class GenericData { self =>

  // This really just so that Intellij doesn't highlight EVERYTHING red.
  protected def defAttr[D]: Attr {type Data = D; def apply(d: D): ValueFor[this.type]} = ???

  /**
   * A data attribute.
   */
  trait AttrBase extends Product with Serializable {
    this: Attr =>
    type Data

    def apply(d: Data): ValueFor[this.type]

    val dataEquality: Eq[Data]

    final def get(vs: Values): Option[ValueFor[this.type]] =
      vs.get(this).asInstanceOf[Option[ValueFor[this.type]]]

    final def areValuesEqual(as: Values, bs: Values): Boolean = {
      val oa = as.get(this)
      val ob = bs.get(this)
      oa ==* ob
    }
  }

  /**
   * A value and the attribute to which it applies.
   */
  trait ValueBase extends Product with Serializable {
    this: Value =>
    val attr: Attr
    val value: attr.Data
  }

  type Attr  <: AttrBase
  type Value <: ValueBase

  type ValueFor[A <: Attr] = Value {val attr: A}
  type Attrs               = NonEmptySet[Attr]
  type Values              = IMap[Attr, Value]
  type NonEmptyValues      = NonEmpty[Values]

  val attrs: Attrs

  implicit val equalityAttr: Order[Attr] with UnivEq[Attr]

  implicit def equalityValue: UnivEq[Value]

  implicit lazy val equalityValues: Eq[Values] =
    IMap.equality

  implicit def equalityNonEmptyValues: Eq[NonEmptyValues] =
    NonEmpty.nonEmptyEqual

  def emptyValues: Values =
    IMap.empty(_.attr)

  def values(vs: Value*): Values =
    emptyValues ++ vs

  def values(vs: IterableOnce[Value]): Values =
    emptyValues ++ vs

  def nev(v1: Value, vn: Value*): NonEmptyValues =
    NonEmpty.force(emptyValues + v1 ++ vn)

  implicit def autoNEV(v: Value): NonEmptyValues =
    NonEmpty.force(emptyValues + v)

  implicit def autoValues(v: Value): Values =
    emptyValues + v

  def removeUnchanged(before: Values, after: Values): Values =
    attrs.foldLeft(after)((vs, a) =>
      if (a.areValuesEqual(before, after))
        vs
      else
        vs - a
    )

  final def valueBuilder(): GenericData.ValueBuilder { val gd: self.type } =
    new GenericData.ValueBuilder {
      override val gd: self.type = self
      override protected var _values = gd.emptyValues
    }

  case class ValueTypeClasses[T[_]](value: T[Value], values: T[Values], nev: T[NonEmptyValues])
}

object GenericData {

  sealed trait ValueBuilder {
    val gd: GenericData

    protected var _values: gd.Values

    def values(): gd.Values =
      _values

    def nev(): Option[gd.NonEmptyValues] =
      NonEmpty(_values)

    def add(a: gd.Attr)(v: a.Data): Unit =
      _values += (a(v))

    def addValue(v: gd.Value): Unit =
      _values += v

    def addValues(vs: IterableOnce[gd.Value]): Unit =
      _values ++= vs

    def addIfChanged(a: gd.Attr)(oldValue: a.Data, newValue: a.Data)(implicit e: Eq[a.Data]): Unit =
      if (!e.eqv(oldValue, newValue))
        _values += a(newValue)

    def addIfChangedOption(a: gd.Attr)(oldValue: Option[a.Data], newValue: a.Data)(implicit e: Eq[a.Data]): Unit =
      if (oldValue.forall(!e.eqv(_, newValue)))
        _values += a(newValue)
  }

}