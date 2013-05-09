package com.beardedlogic.usecase.lib
package field

import net.liftweb.http.Templates
import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._

object Fields {

  val TemplateSource = ClearClearable(Templates("uce" :: Nil).open_!)

  private[field] def Template(id: String) = {
    var t = s"#$id ^^" #> ""
    if (id.startsWith("template-")) t = t & s"#$id [id]" #> (None: Option[String])
    t(TemplateSource)
  }

  //  val DateCreated = TextFieldDef("Date Created", None)
  //  val DateLastUpdated = TextFieldDef("Date Last Updated", None)
  val Actors = TextFieldDef("Actors", None)
  val PreConditions = TextFieldDef("Pre-Conditions", None)
  val PostConditions = TextFieldDef("Post-Conditions", None)
  val UseCaseRelationships = TextFieldDef("Use Case Relationships", None)
  val ConstraintsAndBusinessRules = TextFieldDef("Constraints and Business Rules", None)
  val FrequencyOfUse = TextFieldDef("Frequency of Use", None)
  val SpecialRequirements = TextFieldDef("Special Requirements", None)
  val Assumptions = TextFieldDef("Assumptions", None)
  val NotesAndIssues = TextFieldDef("Notes and Issues", None)

  val DefaultFields: List[FieldDef] =
    Actors ::
      PreConditions ::
      PostConditions ::
      NormalAndAlternateCourseFields ::
      ExceptionCourseFields ::
      UseCaseRelationships ::
      ConstraintsAndBusinessRules ::
      FrequencyOfUse ::
      SpecialRequirements ::
      Assumptions ::
      NotesAndIssues ::
      Nil

}