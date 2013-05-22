package com.beardedlogic.usecase.lib

/**
 * Improvement over Scala's enums.
 *
 * The enum type (container) should be an object that extends this trait.
 *
 * @tparam V The type of this enum's values.
 * @since 21/05/2013
 */
trait Enum[V <: EnumValue] {

  /**
   * Denotes a value in an enumeration.
   */
  trait Value extends EnumValue { self: V => }

  val Values: List[V]

  def apply(ordinal: Int): V = get(ordinal).get

  def apply(name: String): V = get(name).get

  def get(ordinal: Int): Option[V] = Values.find(_.ordinal == ordinal)

  def get(name: String): Option[V] = Values.find(_.toString == name)
}

/**
 * Interface for all enum values.
 *
 * This shouldn't be extended directly. Instead enum values should extend `<EnumType>.Value` which extends this.
 *
 * @since 21/05/2013
 */
trait EnumValue {
  val ordinal: Int
  def toString: String
  def name = toString
}
