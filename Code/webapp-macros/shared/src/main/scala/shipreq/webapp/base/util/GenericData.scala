package shipreq.webapp.base.util

import shipreq.base.util.{NonEmpty, IMap, NonEmptySet, UnivEq}

abstract class GenericData {

  // This really just so that Intellij doesn't highlight EVERYTHING red.
  protected def defAttr[D]: Attr {type Data = D; def apply(d: D): ValueFor[this.type]} = ???

  /**
   * A data attribute.
   */
  trait AttrBase extends Product with Serializable {
    this: Attr =>
    type Data
    def apply(d: Data): ValueFor[this.type]

    final def apply(vs: Values): Option[ValueFor[this.type]] =
      vs.get(this).asInstanceOf[Option[ValueFor[this.type]]]
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

  implicit def equality: UnivEq[Attr]

  def emptyValues: Values =
    IMap.empty(_.attr)

  def values(vs: Value*): Values =
    emptyValues ++ vs

  def nev(v1: Value, vn: Value*): NonEmptyValues =
    NonEmpty.force(emptyValues + v1 ++ vn)

  case class ValueTypeClasses[T[_]](value: T[Value], values: T[Values], nev: T[NonEmptyValues])
}
