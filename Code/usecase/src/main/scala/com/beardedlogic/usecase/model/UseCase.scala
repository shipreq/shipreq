package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{GetResult, SetParameter, StaticQuery => Q}
import scala.slick.session.PositionedParameters
import lib._
import db.DBHelpers._

case class UseCase(
  valueId: Long,
  title: String,
  number: Short,
  fieldListId: Long) extends Value[DataType.UseCase]

case class UseCaseWithValue(
  value: PlainValue[DataType.UseCase],
  title: String,
  number: Short,
  fieldListId: Long) extends Value[DataType.UseCase] {

  final def valueId = value.valueId
}

case class UseCaseSummary(
  number: Short,
  title: String,
  rev: Int,
  updatedAt: String
)

// ---------------------------------------------------------------------------------------------------------------------

object UseCaseAccessor {
  implicit val GetResultPlainValue = ValueAccessor.GetValueResult[DataType.UseCase]
  implicit val GetResultUseCase = GetResult(r => UseCase(r.<<, r.<<, r.<<, r.<<))
  implicit val GetResultUseCaseWithValue = GetResult(r => UseCaseWithValue(GetResultPlainValue(r), r.<<, r.<<, r.<<))
  implicit val GetResultUseCaseSummary = GetResult(r => UseCaseSummary(r.<<, r.<<, r.<<, r.<<))

  implicit object SetParameterUseCaseWithValue extends SetParameter[UseCaseWithValue] {
    def apply(v: UseCaseWithValue, pp: PositionedParameters) {
      pp.setLong(v.valueId)
      pp.setString(v.title)
      pp.setShort(v.number)
      pp.setLong(v.fieldListId)
    }
  }

  val Insert = Q.update[UseCaseWithValue]("INSERT INTO usecase VALUES(?,?,?,?)")

  val NextNumber = "select coalesce(max(number),0)+1 from usecase"
  val InsertNext = Q.query[(Long, String, Long), Short](s"INSERT INTO usecase VALUES(?,?,($NextNumber),?) RETURNING number")

  val Select = Q.query[Long, UseCase]("SELECT id, title, number, field_list_id FROM usecase WHERE id=?")

  val SelectWithValue = Q.query[Long, UseCaseWithValue](s"""
    SELECT v.${ValueAccessor.*}, title, number, field_list_id
    FROM value v, usecase u
    WHERE u.id=? AND v.id = u.id
    """.sql)

  val SelectSummaries = Q.queryNA[UseCaseSummary](s"""
    with t1 as (
      select id, row_number() over (partition by data_id order by rev desc) rn
      from value v
      where data_id in (select id from data where type_id = ${DataType.UseCase.ordinal})
    )
    select number, title, rev, to_iso8601_str(updated_at)
    from usecase u, value v
    where u.id in (select id from t1 where rn = 1)
      and u.id=v.id
    order by number
    """.sql)
}

trait UseCaseAccessor extends DatabaseAccessor {
  self: ValueAccessor with RelationAccessor with FieldValueAccessor with DataAccessor =>

  import UseCaseAccessor._

  def createUseCase(value: PlainValue[DataType.UseCase],
    title: String,
    number: Short,
    fieldList: FieldList): UseCaseWithValue = {

    val uc = UseCaseWithValue(value, title, number, fieldList.valueId)
    Insert.execute(uc)
    uc
  }

  // TODO New-UC has GLOBAL scope.
  // TODO New-UC: Use table locking for mutex?
  // TODO New-UC: Lacking appropriate number uniqueness constraint
  def createInitialUseCase(title: String, fieldList: FieldList): UseCaseWithValue = {
    val v = createInitialValue(DataType.UseCase)
    val number = InsertNext.first(v.valueId, title, fieldList.valueId)
    UseCaseWithValue(v, title, number, fieldList.valueId)
  }

  def findUseCase(valueId: Long): Option[UseCase] = Select.firstOption(valueId)
  def findUseCaseWithValue(valueId: Long): Option[UseCaseWithValue] = SelectWithValue.firstOption(valueId)

  def findAllUseCaseSummaries(): List[UseCaseSummary] = SelectSummaries.list
}
