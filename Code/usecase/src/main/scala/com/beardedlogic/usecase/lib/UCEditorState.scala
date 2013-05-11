package com.beardedlogic.usecase.lib

import field._

/**
 * The entire state of the Use Case Editor.
 */
class UCEditorState() {

  val ucId = 1

  var title = "Untitled"

  val fields = Fields.DefaultFields.map(_.newFieldInstance(this))

  val normalCourseTitleId = fields.collectFirst { case f: NormalAndAlternateCourseFields => f.courses.head }.get.stepTextId
}