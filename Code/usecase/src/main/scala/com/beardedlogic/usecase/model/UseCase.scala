package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{GetResult, SetParameter, StaticQuery => Q}
import scala.slick.session.PositionedParameters
import lib._

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
  self: ValueAccessor with RelationAccessor with FieldValueAccessor with DataAccessor =>

  import UseCaseAccessor._

  def createInitialUseCase(uc: UseCaseCtx): UseCase = db.withTransaction {
    val value = createInitialValue(DataType.UseCase)
    createUseCase(value, uc)
  }

  def createUseCase(data: Data[DataType.UseCase], uc: UseCaseCtx, rev: Revision = LatestRev): UseCase = db.withTransaction {
    val value = createValue(data, rev)
    createUseCase(value, uc)
  }

  private def createUseCase(value: Value[DataType.UseCase], uc: UseCaseCtx): UseCase = {
    val ucModel = UseCase(value.valueId, uc.title, uc.number, uc.fieldList.valueId)
    Insert.execute(ucModel)

    // Save and link to fields
    for (fv <- createInitialFieldValues(uc.fields)) {
      relate_usecase_has_fieldValue(value, fv)
    }

    // TODO shouldn't be here... this all needs changing
    uc.lastSave = Some((value, uc.currentState))

    ucModel
  }

  // SYNC

  // get previous values, if not found save initial
  // compare content

  // compare title + number, set changeDetected flag

  // for each field, sync values
  // Field => check if different, sync values if so. EITHER WAY, return a fieldvalue (or None if blank text), and a changeDetected flag
  // - for text, save new field value
  // - for courses, create new field value, iterate through tree either...
  // --- a) relating to existing if no change
  // --- b) creating a new step, then relating

  // if changeDetected, save new UC & Value, relate to all fieldvalues

  def syncUseCase(ucCtx: UseCaseCtx): Unit = db.withTransaction {

    if (ucCtx.dataRec.isEmpty) {
      ucCtx.dataRec = Some(createData(DataType.UseCase))
      //      dao.createInitialUseCase(this)
      // TODO
    }
    /*
    if (lastSave.isEmpty) {
      dao.createUseCase(dataRec.get, this)
    }

    val lastSavedState = ucCtx.lastSave.get._2
    val currentState = ucCtx.currentState
    var changeDetected = false
    if (currentState.number != lastSavedState.number) changeDetected = true
    if (currentState.title != lastSavedState.title) changeDetected = true

    for (f <- ucCtx.fields; fk = f.fieldKey) {
      val lastFieldState = lastSavedState.fieldStates.get(fk)
      val currentFieldState = currentState.fieldStates.get(fk)
      val (changeDetected2, Option[FieldValue]) = f.compare(lastFieldState, currentFieldState)
      changeDetected ||= changeDetected2
    }
    */
  }

  def findUseCase(valueId: Long): Option[UseCase] = Select.firstOption(valueId)
}
