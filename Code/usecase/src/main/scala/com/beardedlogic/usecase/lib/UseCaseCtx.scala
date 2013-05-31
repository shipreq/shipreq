package com.beardedlogic.usecase
package lib

import net.liftweb.http.CometActor
import field.{NormalAndAlternateCourseFields => NCAC, ExceptionCourseFields => EC, _}
import model._
import msg.MessageCentre
import TypeTags._

class UseCaseCtx(cometActor: CometActor) {

  val msgCentre = new MessageCentre(cometActor)

  var number = 1: Short
  var title = "Untitled"

  // TODO hardcoded fieldlist
  val fieldList = Defaults.FieldList
  val fields = fieldList.fieldKeys.map(k => k.fieldDef.newFieldInstance(this, k))

  val courseFields: List[CourseFields] = fields.collect { case f: CourseFields => f }
  def textFields = fields.collect { case f: TextField => f }
  def ncacField: Option[NCAC] = courseFields.collectFirst { case f: NCAC => f }
  def ecField: Option[EC] = courseFields.collectFirst { case f: EC => f }
  // TODO inefficient UCEditorState.stepLabelMap
  def stepLabelMap: Map[String, String] = courseFields.foldLeft(Map.empty[String, String]) { _ ++ _.stepLabelMap }
  def stepLabelMapProvider = () => stepLabelMap

  val normalCourseTitleId = ncacField.get.courses.head.stepTextId

  private[lib] var _savedSteps = Map.empty[Long_StepDataId, String @@ LocalStepId]
  def savedSteps = _savedSteps

  // -------------------------------------------------------------------------------------------------------------------

  def restoreCheckpoint(checkpoint: UseCaseSaveCheckpoint) {
    // TODO restoreCheckpoint only works before init. Undo not yet supported because no HTML is updated.

    // Set the number & title
    this.number = checkpoint.uc.number
    this.title = checkpoint.uc.title

    var finaliseStateFns: List[() => Unit] = List.empty
    val fields2 = fields.asInstanceOf[List[Field[Any]]]
    for (f <- fields2) {
      val state = checkpoint.fieldStates.get(f.fieldKey)
      if (state.isDefined) {
        finaliseStateFns :+= f.setState(state.get)
      }
//    If S didn't exist
//      F.state = None
//        Clear courses, stepLabelMap, textFields
    }

//    Build ucCtx . map of stepDataId  →  Step Node ID
    _savedSteps = checkpoint.saveCtx.stepValues.map{
      case (localStepId, stepValue) => (stepValue.taggedDataId -> localStepId)
    }.toMap

    for (fn <- finaliseStateFns) fn()
  }
}

case class UseCaseSaveCheckpoint(
  uc: UseCaseWithValue,
  saveCtx: FieldSaveCtx,
  fieldStates: Map[FieldKey, Any]
  )

object UseCaseLoader {

  def loadCheckpoint(valueId: Long, dao: DAO): Option[UseCaseSaveCheckpoint] = {

    // Load use case
    dao.findUseCaseWithValue(valueId).map { uc =>
      val fieldList = Defaults.FieldList // TODO hardcoded fieldlist

      val saveCtx = new MutableFieldSaveCtx
      // TODO populate field values

      // Load field states
      val loadCtx = dao.getFieldLoadCtxFor(valueId)
      val fieldStates = Map.newBuilder[FieldKey, Any]
      for (fk <- fieldList.fieldKeys) {
        val fs = fk.fieldDef.stateLoader(fk).load(loadCtx, saveCtx)
        fieldStates += (fk -> fs)
      }

      UseCaseSaveCheckpoint(uc, saveCtx.immutable, fieldStates.result)
    }
  }
}
