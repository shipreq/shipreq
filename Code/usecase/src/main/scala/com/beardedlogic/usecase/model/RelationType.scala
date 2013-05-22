package com.beardedlogic.usecase.model

/**
 * Represents each possible type of relationship that can exist between two data values.
 *
 * @since 22/05/2013
 */
sealed abstract class RelationType(val ordinal: Int) extends RelationType.Value
object RelationType extends DatabaseEnum[RelationType] {

  override val TableName = "relation_type"

  /**
   * A includes B in its definition.
   * B is a part of A.
   */
  case object Has extends RelationType(200)

  /**
   * A refers to B.
   * A knows of B's existence and depends on, or is interested in its value.
   */
  case object References extends RelationType(201)

  val Values = List(Has, References)
}