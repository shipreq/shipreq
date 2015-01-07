package shipreq.webapp.base.test

import shipreq.prop.util._
import shipreq.webapp.base.data._, DataImplicits._
import shipreq.webapp.base.UnsafeTypes._

object SampleProject {

  lazy val customIssueTypes = RevAnd(10, emptyDataMap(CustomIssueType).addAll(
    CustomIssueType(1, "TO"+"DO", "Something you need To Do.", Alive),
    CustomIssueType(2, "TBD", "To Be Decided.", Alive)))

  lazy val customReqTypes = RevAnd(20, emptyDataMap(CustomReqType).addAll(
    CustomReqType(1, "CO", Set.empty, "Constraint",             ImplicationRequired.Not, Alive),
    CustomReqType(2, "MF", Set.empty, "Major Feature",          ImplicationRequired.Not, Alive),
    CustomReqType(3, "FR", Set.empty, "Functional Requirement", ImplicationRequired,     Alive),
    CustomReqType(4, "BR", Set.empty, "Business Rule",          ImplicationRequired.Not, Alive),
    CustomReqType(5, "DD", Set("DA", "DDF"), "Data Definition", ImplicationRequired.Not, Dead),
    CustomReqType(6, "SI", Set.empty, "Solution Idea",          ImplicationRequired,     Dead)))

  lazy val v10d = Some("Released: 17/14/1976\nFirst release.")
  lazy val v11d = Some("Released: 1/2/2001")
  lazy val tags = TagTree.empty.addAll(
    TagInTree(TagGroup     (1, "Priority",        None, MutexChildren,     Alive), Vector(2,3,4)),
    TagInTree(ApplicableTag(2, "High Priority",   None, "pri=high",        Alive), Vector()),
    TagInTree(ApplicableTag(3, "Medium Priority", None, "pri=med",         Alive), Vector()),
    TagInTree(TagGroup     (10, "Status",         None, MutexChildren.Not, Alive), Vector(11,12)),
    TagInTree(ApplicableTag(11, "WIP",            None, "wip",             Alive), Vector()),
    TagInTree(ApplicableTag(12, "Deferred",       None, "defer",           Alive), Vector()),
    TagInTree(TagGroup     (20, "Version",        None, MutexChildren.Not, Alive), Vector(27,21,25,26)),
    TagInTree(ApplicableTag(21, "v1.x",           None, "v1.x",            Alive), Vector(22,23,24)),
    TagInTree(ApplicableTag(22, "v1.0",           v10d, "v1.0",            Alive), Vector()),
    TagInTree(ApplicableTag(23, "v1.1",           v11d, "v1.1",            Alive), Vector()),
    TagInTree(ApplicableTag(24, "v1.2",           None, "v1.2",            Alive), Vector()),
    TagInTree(ApplicableTag(25, "v2.x",           None, "v2.x",            Alive), Vector()),
    TagInTree(ApplicableTag(26, "v3.x",           None, "v3.x",            Dead ), Vector()),
    TagInTree(TagGroup     (27, "Released",       None, MutexChildren.Not, Alive), Vector(22,23)),
    TagInTree(ApplicableTag(4, "Low Priority", Some("Nice to have. Stuff that probably won't be implemented."), "pri=low", Alive), Vector()))


  lazy val fields = RevAnd(40, FieldSet(emptyDataMap(CustomField).addAll(
    CustomField.Text(1, "Description", "desc",     Mandatory,     onlyReqTypes(2, ReqType.UseCase), Alive),
    CustomField.Text(2, "Notes",       "notes",    Mandatory.Not, notReqTypes(4),                   Alive),
    CustomField.Text(3, "Reporter",    "reporter", Mandatory,     onlyReqTypes(5, ReqType.UseCase), Dead)
  ), Vector(
    1, 3, StaticField.NormalAltStepTree, StaticField.ExceptionStepTree, StaticField.StepGraph, 2
  )))

  lazy val project = new Project(
    customIssueTypes,
    customReqTypes,
    fields,
    RevAnd(30, tags))

  lazy val tagTree = project.tags.data.mapValues(_.children)

  // lazy val tagTreeB = BiMultimap(Multimap(tagTree.mapValues(_.toSet)))
}
