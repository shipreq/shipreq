package shipreq.webapp.base.test

import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.Event

/**
 * Sample project with:
 *    - sample config
 *    - no content
 *
 * ReqTypes:
 *    - live: BR, CO, FR, MF, UC
 *    - dead: DD, SI
 */
object SampleProject {
  import DataImplicits._
  import WebappTestUtil._
  import UnsafeTypes._

  trait Values {
    def uc = StaticReqType.UseCase
    val List(co, mf, fr, br, dd, si)                      = List[CustomReqTypeId           ](1, 2, 3, 4, 5, 6)
    val List(priHigh, priMed, priLow)                     = List[ApplicableTagId           ](2, 3, 4)
    val List(wip, defer, uat, uat2, uat3, prod)           = List[ApplicableTagId           ](11, 12, 13, 14, 15, 16)
    val List(v09, v10, v11, v12, v13, v1x, v2x, v3x, v4x) = List[ApplicableTagId           ](28, 22, 23, 24, 29, 21, 25, 26, 30)
    val List(descField, notesField, reporterField)        = List[CustomField.Text.Id       ](1, 2, 3)
    val List(priField, statusField, verField, relField)   = List[CustomField.Tag.Id        ](4, 5, 20, 7)
    val List(mfField)                                     = List[CustomField.Implication.Id](6)

    val allLiveTags = NonEmptyVector(priHigh, priMed, priLow, wip, defer, v10, v11, v12, v13, v1x, v2x)
    val allDeadTags = NonEmptyVector(uat, uat2, v09, v3x, v4x)

    val priTG    = 1.TG
    val statusTG = 10.TG
    val verTG    = 20.TG
    val relTG    = 27.TG
  }

  object Values extends Values
  import Values._

  lazy val customIssueTypes = emptyDataMap(CustomIssueType).addAll(
    CustomIssueType(1, "TO"+"DO", "Something you need To Do.", Live),
    CustomIssueType(2, "TBD", "To Be Decided.", Live),
    CustomIssueType(3, "PENDING", "Just pendin'", Dead))

  lazy val customReqTypes = emptyDataMap(CustomReqType).addAll(
    CustomReqType.v1(co, "CO", Set.empty, "Constraint",             Optional,  Live),
    CustomReqType.v1(mf, "MF", Set.empty, "Major Feature",          Optional,  Live),
    CustomReqType.v1(fr, "FR", Set.empty, "Functional Requirement", Mandatory, Live),
    CustomReqType.v1(br, "BR", Set.empty, "Business Rule",          Optional,  Live),
    CustomReqType.v1(dd, "DD", Set("DA", "DDF"), "Data Definition", Optional,  Dead),
    CustomReqType.v1(si, "SI", Set.empty, "Solution Idea",          Mandatory, Dead))

  lazy val v10d = Some("Released: 17/14/1976\nFirst release.")
  lazy val v11d = Some("Released: 1/2/2001")
  lazy val tags = TagTree.empty.addAll(
    TagInTree(TagGroup        (priTG   , "Priority",        None, Exclusive,    Live), Vector(priHigh, priMed, priLow)),
    TagInTree(ApplicableTag.v1(priHigh , "High Priority",   None, "pri=high",   Live), Vector()),
    TagInTree(ApplicableTag.v1(priMed  , "Medium Priority", None, "pri=med",    Live), Vector()),
    TagInTree(TagGroup        (statusTG, "Status",          None, NonExclusive, Live), Vector(wip, defer, uat, uat2, uat3, prod)),
    TagInTree(ApplicableTag.v1(wip     , "WIP",             None, "wip",        Live), Vector()),
    TagInTree(ApplicableTag.v1(defer   , "Deferred",        None, "defer",      Live), Vector()),
    TagInTree(ApplicableTag.v1(uat     , "In UAT #1",       None, "uat",        Dead), Vector()),
    TagInTree(ApplicableTag.v1(uat2    , "In UAT #2",       None, "uat2",       Dead), Vector()),
    TagInTree(ApplicableTag.v1(uat3    , "In UAT #3",       None, "uat3",       Dead), Vector()),
    TagInTree(ApplicableTag.v1(prod    , "In Production",   None, "prod",       Live), Vector()),
    TagInTree(TagGroup        (verTG   , "Version",         None, NonExclusive, Live), Vector(relTG, v1x, v2x, v3x, v4x)),
    TagInTree(ApplicableTag.v1(v1x     , "v1.x",            None, "v1.x",       Live), Vector(v10, v11, v12, v13)),
    TagInTree(ApplicableTag.v1(v10     , "v1.0",            v10d, "v1.0",       Live), Vector()),
    TagInTree(ApplicableTag.v1(v11     , "v1.1",            v11d, "v1.1",       Live), Vector()),
    TagInTree(ApplicableTag.v1(v12     , "v1.2",            None, "v1.2",       Live), Vector()),
    TagInTree(ApplicableTag.v1(v13     , "v1.3",            None, "v1.3",       Live), Vector()),
    TagInTree(ApplicableTag.v1(v2x     , "v2.x",            None, "v2.x",       Live), Vector()),
    TagInTree(ApplicableTag.v1(v3x     , "v3.x",            None, "v3.x",       Dead), Vector()),
    TagInTree(ApplicableTag.v1(v4x     , "v4.x",            None, "v4.x",       Dead), Vector()),
    TagInTree(TagGroup        (relTG   , "Released",        None, NonExclusive, Live), Vector(v09, v10, v11)),
    TagInTree(ApplicableTag.v1(v09     , "v0.9",            None, "v0.9",       Dead), Vector()),
    TagInTree(ApplicableTag.v1(priLow  , "Low Priority", Some("Nice to have. Stuff that probably won't be implemented."), "pri=low", Live), Vector()))

  lazy val fields = {
    import CustomField._
    FieldSet(emptyDataMap(CustomField).addAll(
      Text       .v1(descField    , "Description", "desc",     Optional,  onlyReqTypes(mf, si, StaticReqType.UseCase), Live),
      Text       .v1(notesField   , "Notes",       "notes",    Optional,  notReqTypes(br),                             Live),
      Text       .v1(reporterField, "Reporter",    "reporter", Mandatory, onlyReqTypes(br, dd, StaticReqType.UseCase), Dead),
      Tag        .v1(priField     , priTG,                     Mandatory, allReqTypes,                                 Live),
      Tag        .v1(statusField  , statusTG,                  Optional,  notReqTypes(dd, si),                         Live),
      Implication.v1(mfField      , mf,                        Optional,  notReqTypes(si),                             Live),
      Tag        .v1(relField     , relTG,                     Optional,  allReqTypes,                                 Dead)
    ), Vector(
      descField,
      mfField,
      priField,
      reporterField,
      StaticField.NormalAltStepTree,
      StaticField.ExceptionStepTree,
      StaticField.StepGraph,
      relField,
      statusField,
      notesField
    ))
  }

  lazy val reqs     = Requirements.empty
  lazy val reqCodes = ReqCodes.empty
  lazy val reqText  = ReqData.emptyText
  lazy val reqTags  = ReqData.emptyTags
  lazy val reqImps  = Implications.emptyBiDir

  lazy val projectConfig = ProjectConfig(customIssueTypes, ReqTypes(customReqTypes), fields, tags)
  lazy val projectContent = ProjectContent(reqs, reqCodes, reqText, reqTags, reqImps, DeletionReasons.empty)

  lazy val project = IdCeilings.supply(
    Project(
      "Sample Project",
      projectConfig,
      projectContent,
      ManualIssues.empty,
      reqtable.SavedViews.empty,
      _,
      None))

  lazy val tagTree = project.config.tags.tree.mapValues(_.children)

  lazy val projectWithOtherTags =
    applyEventSuccessfully(project, Event.FieldStaticAdd(StaticField.OtherTags))
}
