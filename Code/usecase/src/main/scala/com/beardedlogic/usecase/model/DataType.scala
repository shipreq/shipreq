package com.beardedlogic.usecase.model

import com.beardedlogic.usecase.lib.db.DatabaseEnum

/**
 * Represents each possible type of data that can be stored in the `data` table.
 *
 * Accordingly each type declared here is also versionable, and can be dynamically and loosely linked to other data.
 *
 * @since 22/05/2013
 */
sealed abstract class DataType(val ordinal: Short) extends DataType.Value

object DataType extends DatabaseEnum[DataType] {

  override val TableName = "data_type"

  /**
   * A single, isolated use case.
   *
   * Example: `UC-1: User logs in.`
   */
  case object UseCase extends DataType(100)
  type UseCase = UseCase.type

  /**
   * An ordered list of a fields.
   *
   * Examples:
   * - Actors
   * - Pre-conditions
   * - Post-conditions
   * - Notes and Issues
   */
  case object FieldList extends DataType(101)
  type FieldList = FieldList.type

  /**
   * A single or composite field definition. A collection of these would make a template.
   *
   * Examples:
   * - Triggers
   * - Pre-conditions
   * - Normal Courses & Alternate Courses
   */
  case object FieldKey extends DataType(102)
  type FieldKey = FieldKey.type

  /**
   * A single or composite value to a corresponding [[com.beardedlogic.usecase.model.DataType.FieldKey]].
   */
  case object FieldValue extends DataType(103) with StepParent
  type FieldValue = FieldValue.type

  /**
   * A single node in a tree of steps.
   *
   * Children are specified using [[com.beardedlogic.usecase.model.RelationType.Has]]
   */
  case object Step extends DataType(104) with StepParent
  type Step = Step.type

  val Values = List(UseCase, FieldList, FieldKey, FieldValue, Step)
}