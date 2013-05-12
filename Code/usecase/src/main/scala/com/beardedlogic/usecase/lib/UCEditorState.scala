package com.beardedlogic.usecase.lib

import net.liftweb.http.CometActor
import field.Fields.DefaultFields
import field.{CourseFields, NormalAndAlternateCourseFields => NCAC}
import msg.MessageCentre

/**
 * The entire state of the Use Case Editor.
 */
class UCEditorState(cometActor: CometActor) {

  val ucId = 1

  var title = "Untitled"

  val msgCentre = new MessageCentre(cometActor)

  val fields = DefaultFields.map(_.newFieldInstance(this))

  val courseFields: List[CourseFields] = fields.collect { case f: CourseFields => f }

  val normalCourseTitleId = courseFields.collectFirst { case f: NCAC => f.courses.head }.get.stepTextId

  // TODO inefficient UCEditorState.stepLabelMap
  def stepLabelMap = courseFields.foldLeft(Map.empty[String, String]) { _ ++ _.stepLabelMap }

  def stepLabelMapProvider = () => stepLabelMap
}