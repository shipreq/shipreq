package com.beardedlogic.usecase
package lib

import net.liftweb.http.CometActor
import field.{CourseFields, TextField, NormalAndAlternateCourseFields => NCAC, ExceptionCourseFields => EC}
import msg.MessageCentre
import model.DAO

/**
 * The entire state of the Use Case Editor.
 */
class UCEditorState(
  val ucNumber: Short,
  cometActor: CometActor) {

  val msgCentre = new MessageCentre(cometActor)

  var title = "Untitled"

  val fieldList = Defaults.FieldList

  val fields = fieldList.fieldKeys.map(k => k.fieldDef.newFieldInstance(this, k))

  val courseFields: List[CourseFields] = fields.collect { case f: CourseFields => f }

  def textFields = fields.collect { case f: TextField => f }
  def ncacField: Option[NCAC] = courseFields.collectFirst { case f: NCAC => f }
  def ecField: Option[EC] = courseFields.collectFirst { case f: EC => f }

  val normalCourseTitleId = ncacField.get.courses.head.stepTextId

  // TODO inefficient UCEditorState.stepLabelMap
  def stepLabelMap = courseFields.foldLeft(Map.empty[String, String]) { _ ++ _.stepLabelMap }

  def stepLabelMapProvider = () => stepLabelMap
}

object UCEditorState {

  def load(valueId: Long, cometActor: CometActor, dao: DAO): Option[UCEditorState] =
    dao.findUseCase(valueId).map { uc =>
      val uce = new UCEditorState(uc.number, cometActor)
      uce.title = uc.title
      val ctx = dao.getFieldLoadCtxFor(valueId)
      for (f <- uce.fields) f.load(ctx)
      uce
    }
}