package com.beardedlogic.usecase.lib
package field

import scala.xml.NodeSeq
import net.liftweb.http.Templates
import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._

object Fields {

  val TemplateSource = ClearClearable(Templates("uce" :: Nil) openOrThrowException "UC Editor template not found.")

  private[field] def Template(id: String): NodeSeq = {
    var t = s"#$id ^^" #> ""
    if (id.startsWith("template-")) t = t & s"#$id [id]" #> (None: Option[String])
    val r = t(TemplateSource)
    if (r.isEmpty) throw new Exception(s"Failed to load template: $id\nTemplateSource.length = ${TemplateSource.length}")
    r
  }
}

// TODO Move elsewhere
object Defaults {

  //  val DateCreated = TextFieldDef("Date Created")
  //  val DateLastUpdated = TextFieldDef("Date Last Updated")
  val Actors = TextFieldDef("Actors")
  val PreConditions = TextFieldDef("Pre-Conditions")
  val PostConditions = TextFieldDef("Post-Conditions")
  val UseCaseRelationships = TextFieldDef("Use Case Relationships")
  val ConstraintsAndBusinessRules = TextFieldDef("Constraints and Business Rules")
  val FrequencyOfUse = TextFieldDef("Frequency of Use")
  val SpecialRequirements = TextFieldDef("Special Requirements")
  val Assumptions = TextFieldDef("Assumptions")
  val NotesAndIssues = TextFieldDef("Notes and Issues")

  // TODO Rename Defaults.DefaultFields
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