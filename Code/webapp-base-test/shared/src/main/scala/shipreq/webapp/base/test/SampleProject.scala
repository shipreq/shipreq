package shipreq.webapp.base.test

import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.base.util.ISubset
import shipreq.webapp.base.data._
import DataImplicits._
import UnsafeTypes._

/**
 * Sample project with:
 *    - sample config
 *    - no content
 */
object SampleProject {

  trait Values {
    val List(co, mf, fr, br, dd, si)                      = List[CustomReqTypeId           ](1, 2, 3, 4, 5, 6)
    val List(priHigh, priMed, priLow)                     = List[ApplicableTagId           ](2, 3, 4)
    val List(wip, defer, uat, uat2, uat3, prod)           = List[ApplicableTagId           ](11, 12, 13, 14, 15, 16)
    val List(v09, v10, v11, v12, v13, v1x, v2x, v3x, v4x) = List[ApplicableTagId           ](28, 22, 23, 24, 29, 21, 25, 26, 30)
    val List(descField, notesField, reporterField)        = List[CustomField.Text.Id       ](1, 2, 3)
    val List(priField, statusField, verField, relField)   = List[CustomField.Tag.Id        ](4, 5, 20, 7)
    val List(mfField)                                     = List[CustomField.Implication.Id](6)

    val allLiveTags = NonEmptyVector(priHigh, priMed, priLow, wip, defer, v10, v11, v12, v13, v1x, v2x)
    val allDeadTags = NonEmptyVector(uat, uat2, v09, v3x, v4x)
  }

  object Values extends Values
  import Values._

  lazy val customIssueTypes = emptyDataMap(CustomIssueType).addAll(
    CustomIssueType(1, "TO"+"DO", "Something you need To Do.", Live),
    CustomIssueType(2, "TBD", "To Be Decided.", Live),
    CustomIssueType(3, "PENDING", "Just pendin'", Dead))

  lazy val customReqTypes = emptyDataMap(CustomReqType).addAll(
    CustomReqType(co, "CO", Set.empty, "Constraint",             ImplicationRequired.Not, Live),
    CustomReqType(mf, "MF", Set.empty, "Major Feature",          ImplicationRequired.Not, Live),
    CustomReqType(fr, "FR", Set.empty, "Functional Requirement", ImplicationRequired,     Live),
    CustomReqType(br, "BR", Set.empty, "Business Rule",          ImplicationRequired.Not, Live),
    CustomReqType(dd, "DD", Set("DA", "DDF"), "Data Definition", ImplicationRequired.Not, Dead),
    CustomReqType(si, "SI", Set.empty, "Solution Idea",          ImplicationRequired,     Dead))

  lazy val v10d = Some("Released: 17/14/1976\nFirst release.")
  lazy val v11d = Some("Released: 1/2/2001")
  lazy val tags = TagTree.empty.addAll(
    TagInTree(TagGroup     (1      , "Priority",        None, MutexChildren,     Live), Vector(priHigh, priMed, priLow)),
    TagInTree(ApplicableTag(priHigh, "High Priority",   None, "pri=high",        Live), Vector()),
    TagInTree(ApplicableTag(priMed , "Medium Priority", None, "pri=med",         Live), Vector()),
    TagInTree(TagGroup     (10     , "Status",          None, MutexChildren.Not, Live), Vector(wip, defer, uat, uat2, uat3, prod)),
    TagInTree(ApplicableTag(wip    , "WIP",             None, "wip",             Live), Vector()),
    TagInTree(ApplicableTag(defer  , "Deferred",        None, "defer",           Live), Vector()),
    TagInTree(ApplicableTag(uat    , "In UAT #1",       None, "uat",             Dead), Vector()),
    TagInTree(ApplicableTag(uat2   , "In UAT #2",       None, "uat2",            Dead), Vector()),
    TagInTree(ApplicableTag(uat3   , "In UAT #3",       None, "uat3",            Dead), Vector()),
    TagInTree(ApplicableTag(prod   , "In Production",   None, "prod",            Live), Vector()),
    TagInTree(TagGroup     (20     , "Version",         None, MutexChildren.Not, Live), Vector(27.TG, v1x, v2x, v3x, v4x)),
    TagInTree(ApplicableTag(v1x    , "v1.x",            None, "v1.x",            Live), Vector(v10, v11, v12, v13)),
    TagInTree(ApplicableTag(v10    , "v1.0",            v10d, "v1.0",            Live), Vector()),
    TagInTree(ApplicableTag(v11    , "v1.1",            v11d, "v1.1",            Live), Vector()),
    TagInTree(ApplicableTag(v12    , "v1.2",            None, "v1.2",            Live), Vector()),
    TagInTree(ApplicableTag(v13    , "v1.3",            None, "v1.3",            Live), Vector()),
    TagInTree(ApplicableTag(v2x    , "v2.x",            None, "v2.x",            Live), Vector()),
    TagInTree(ApplicableTag(v3x    , "v3.x",            None, "v3.x",            Dead), Vector()),
    TagInTree(ApplicableTag(v4x    , "v4.x",            None, "v4.x",            Dead), Vector()),
    TagInTree(TagGroup     (27     , "Released",        None, MutexChildren.Not, Live), Vector(v09, v10, v11)),
    TagInTree(ApplicableTag(v09    , "v0.9",            None, "v0.9",            Dead), Vector()),
    TagInTree(ApplicableTag(priLow , "Low Priority", Some("Nice to have. Stuff that probably won't be implemented."), "pri=low", Live), Vector()))

  lazy val fields = {
    import CustomField._
    FieldSet(emptyDataMap(CustomField).addAll(
      Text       (descField    , "Description", "desc",     Mandatory,     onlyReqTypes(mf, si, StaticReqType.UseCase), Live),
      Text       (notesField   , "Notes",       "notes",    Mandatory.Not, notReqTypes(br),                             Live),
      Text       (reporterField, "Reporter",    "reporter", Mandatory,     onlyReqTypes(dd, StaticReqType.UseCase),     Dead),
      Tag        (priField     , 1.TG,                      Mandatory,     ISubset.All(),                               Live),
      Tag        (statusField  , 10.TG,                     Mandatory.Not, notReqTypes(dd, si),                         Live),
      Implication(mfField      , mf,                        Mandatory.Not, notReqTypes(si),                             Live),
      Tag        (relField     , 27.TG,                     Mandatory.Not, ISubset.All(),                               Dead)
    ), Vector(
      descField, mfField, priField, reporterField,
      StaticField.NormalAltStepTree, StaticField.ExceptionStepTree, StaticField.StepGraph,
      relField, statusField, notesField
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
      reqtable.SavedViews.empty,
      _))

  lazy val tagTree = project.config.tags.tree.mapValues(_.children)

  // lazy val tagTreeB = BiMultimap(Multimap(tagTree.mapValues(_.toSet)))
}
