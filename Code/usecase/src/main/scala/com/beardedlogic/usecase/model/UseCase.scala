package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{GetResult, SetParameter, StaticQuery => Q}
import scala.slick.session.PositionedParameters
import lib.{Defaults, UCEditorState}

case class UseCase(
  valueId: Long,
  title: String,
  number: Short,
  fieldListId: Long) extends Value[DataType.UseCase]

object UseCaseAccessor {
  implicit val GetResultUseCase = GetResult(r => UseCase(r.<<, r.<<, r.<<, r.<<))

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
}

trait UseCaseAccessor extends DatabaseAccessor {
  self: ValueAccessor with RelationAccessor with FieldValueAccessor =>

  import UseCaseAccessor._

  def createInitialUseCase(uc: UCEditorState): UseCase = db.withTransaction {
    // Save UC
    val ucValue = createInitialValue(DataType.UseCase)
    val ucModel = UseCase(ucValue.valueId, uc.title, uc.ucNumber.toShort, uc.fieldList.valueId)
    Insert.execute(ucModel)

    // Save and link to fields
    for (fv <- createInitialFieldValues(uc.fields)) {
      relate_usecase_has_fieldValue(ucValue, fv)
    }

    ucModel
  }

  def findUseCase(valueId: Long): Option[UseCase] = Select.firstOption(valueId)
}
