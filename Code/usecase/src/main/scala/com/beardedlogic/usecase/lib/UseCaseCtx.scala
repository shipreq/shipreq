package com.beardedlogic.usecase
package lib

import model._
import net.liftweb.http.CometActor
import field.{Field, CourseFields, TextField, NormalAndAlternateCourseFields => NCAC, ExceptionCourseFields => EC}
import msg.MessageCentre
import UseCaseCtx._

case class UseCaseState(
  val number: Short,
  val title: String,
  val fieldStates: Map[FieldKey, Any])

object UseCaseCtx {
  @inline def getFieldStates(fields: List[Field[_]]): Map[FieldKey, Any] = fields.map { f => (f.fieldKey, f.state) }.toMap
}

class UseCaseCtx(cometActor: CometActor) {

  val msgCentre = new MessageCentre(cometActor)

  /** If saved, this is the data record. **/
  var dataRec: Option[Data[DataType.UseCase]] = None

  var lastSave: Option[(Value[DataType.UseCase], UseCaseState)] = None

  var number = 1: Short
  var title = "Untitled"
  def currentFieldValues: Map[FieldKey, Any] = getFieldStates(fields)
  def currentState = UseCaseState(number, title, currentFieldValues)

  // TODO hardcoded fieldlist
  val fieldList = Defaults.FieldList
  val fields = fieldList.fieldKeys.map(k => k.fieldDef.newFieldInstance(this, k))

  val courseFields: List[CourseFields] = fields.collect { case f: CourseFields => f }
  def textFields = fields.collect { case f: TextField => f }
  def ncacField: Option[NCAC] = courseFields.collectFirst { case f: NCAC => f }
  def ecField: Option[EC] = courseFields.collectFirst { case f: EC => f }
  // TODO inefficient UCEditorState.stepLabelMap
  def stepLabelMap = courseFields.foldLeft(Map.empty[String, String]) { _ ++ _.stepLabelMap }
  def stepLabelMapProvider = () => stepLabelMap

  val normalCourseTitleId = ncacField.get.courses.head.stepTextId

  // ---------------------------------------------------------------------------------------------------

  def load(valueId: Long, dao: DAO): Unit = synchronized {
    val loaded = loadStateWithoutApplying(valueId, dao)
    loaded.foreach { case (uc, state) =>
      // TODO dataRec =
      lastSave = loaded
      setState(state)
    }
  }

  def loadStateWithoutApplying(valueId: Long, dao: DAO): Option[(Value[DataType.UseCase], UseCaseState)] =
    dao.findUseCase(valueId).map { uc =>
      val fieldList = Defaults.FieldList // TODO hardcoded fieldlist

      var fieldStates = Map.newBuilder[FieldKey, Any]
      val ctx = dao.getFieldLoadCtxFor(valueId)
      for (fk <- fieldList.fieldKeys) {
        val v = fk.fieldDef.stateLoader(fk).load(ctx)
        fieldStates += (fk -> v)
      }

      (uc, UseCaseState(uc.number, uc.title, fieldStates.result))
    }

  def setState(s: UseCaseState): Unit = synchronized {
    this.title = s.title
    this.number = s.number
    for {
      f <- fields
      v <- s.fieldStates.get(f.fieldKey)
    } f.asInstanceOf[Field[Any]].state = v
  }

  def save(dao: DAO):Unit = synchronized { dao.withTransaction {
    if (dataRec.isEmpty) {
      dataRec = Some(dao.createData(DataType.UseCase))
//      dao.createInitialUseCase(this)
    }
    if (lastSave.isEmpty) {
      dao.createUseCase(dataRec.get, this)
    }
  }}
}