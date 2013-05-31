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

// ---------------------------------------------------------------------------------------------------------------------

object UseCaseAccessor {
  implicit val GetResultPlainValue = ValueAccessor.GetValueResult[DataType.UseCase]
  implicit val GetResultUseCase = GetResult(r => UseCase(r.<<, r.<<, r.<<, r.<<))
  implicit val GetResultUseCaseWithValue = GetResult(r => UseCaseWithValue(GetResultPlainValue(r), r.<<, r.<<, r.<<))

  implicit object SetParameterUseCase extends SetParameter[UseCase] {
    def apply(v: UseCase, pp: PositionedParameters) {
      pp.setLong(v.valueId)
      pp.setString(v.title)
      pp.setShort(v.number)
      pp.setLong(v.fieldListId)
    }
  }

  val Insert = Q.update[UseCase]("INSERT INTO usecase VALUES(?,?,?,?)")
  val Select = Q.query[Long, UseCase]("SELECT id, title, number, field_list_id FROM usecase WHERE id=?")
  val SelectWithValue = Q.query[Long, UseCaseWithValue](s"""
    SELECT v.${ValueAccessor.*}, title, number, field_list_id
    FROM value v, usecase u
    WHERE u.id=? AND v.id = u.id
    """.sql)
}

trait UseCaseAccessor extends DatabaseAccessor {
  self: ValueAccessor with RelationAccessor with FieldValueAccessor with DataAccessor =>

  import UseCaseAccessor._

  def createUseCase(data: Data[DataType.UseCase], uc: UseCaseCtx, rev: Revision = LatestRev): UseCase = db.withTransaction {
    val value = createValue(data, rev)
    val ucModel = UseCase(value.valueId, uc.title, uc.number, uc.fieldList.valueId)
    Insert.execute(ucModel)
    ucModel
  }

  def findUseCase(valueId: Long): Option[UseCase] = Select.firstOption(valueId)
  def findUseCaseWithValue(valueId: Long): Option[UseCaseWithValue] = SelectWithValue.firstOption(valueId)
}
