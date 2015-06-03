package shipreq.webapp.base.test

import japgolly.nyaya.util.Multimap
import shipreq.webapp.base.data._, DataImplicits._
import UnsafeTypes._

object SampleProject {

  lazy val customIssueTypes = RevAnd(10, emptyDataMap(CustomIssueType).addAll(
    CustomIssueType(1, "TO"+"DO", "Something you need To Do.", Alive),
    CustomIssueType(2, "TBD", "To Be Decided.", Alive),
    CustomIssueType(3, "PENDING", "Just pendin'", Dead)))

  lazy val customReqTypes = RevAnd(20, emptyDataMap(CustomReqType).addAll(
    CustomReqType(1, "CO", Set.empty, "Constraint",             ImplicationRequired.Not, Alive),
    CustomReqType(2, "MF", Set.empty, "Major Feature",          ImplicationRequired.Not, Alive),
    CustomReqType(3, "FR", Set.empty, "Functional Requirement", ImplicationRequired,     Alive),
    CustomReqType(4, "BR", Set.empty, "Business Rule",          ImplicationRequired.Not, Alive),
    CustomReqType(5, "DD", Set("DA", "DDF"), "Data Definition", ImplicationRequired.Not, Dead),
    CustomReqType(6, "SI", Set.empty, "Solution Idea",          ImplicationRequired,     Dead)))

  lazy val tagsR = RevAnd(30, tags)
  lazy val v10d = Some("Released: 17/14/1976\nFirst release.")
  lazy val v11d = Some("Released: 1/2/2001")
  lazy val tags = TagTree.empty.addAll(
    TagInTree(TagGroup     (1, "Priority",        None, MutexChildren,     Alive), Vector(2.AT, 3.AT, 4.AT)),
    TagInTree(ApplicableTag(2, "High Priority",   None, "pri=high",        Alive), Vector()),
    TagInTree(ApplicableTag(3, "Medium Priority", None, "pri=med",         Alive), Vector()),
    TagInTree(TagGroup     (10, "Status",         None, MutexChildren.Not, Alive), Vector(11.AT, 12.AT, 13.AT)),
    TagInTree(ApplicableTag(11, "WIP",            None, "wip",             Alive), Vector()),
    TagInTree(ApplicableTag(12, "Deferred",       None, "defer",           Alive), Vector()),
    TagInTree(ApplicableTag(13, "In UAT",         None, "uat",             Dead ), Vector()),
    TagInTree(TagGroup     (20, "Version",        None, MutexChildren.Not, Alive), Vector(27.TG, 21.AT, 25.AT, 26.AT)),
    TagInTree(ApplicableTag(21, "v1.x",           None, "v1.x",            Alive), Vector(22.AT, 23.AT, 24.AT)),
    TagInTree(ApplicableTag(22, "v1.0",           v10d, "v1.0",            Alive), Vector()),
    TagInTree(ApplicableTag(23, "v1.1",           v11d, "v1.1",            Alive), Vector()),
    TagInTree(ApplicableTag(24, "v1.2",           None, "v1.2",            Alive), Vector()),
    TagInTree(ApplicableTag(25, "v2.x",           None, "v2.x",            Alive), Vector()),
    TagInTree(ApplicableTag(26, "v3.x",           None, "v3.x",            Dead ), Vector()),
    TagInTree(TagGroup     (27, "Released",       None, MutexChildren.Not, Alive), Vector(28.AT, 22.AT, 23.AT)),
    TagInTree(ApplicableTag(28, "v0.9",           None, "v0.9",            Dead ), Vector()),
    TagInTree(ApplicableTag(4, "Low Priority", Some("Nice to have. Stuff that probably won't be implemented."), "pri=low", Alive), Vector()))

  lazy val fields = {
    import CustomField._
    RevAnd(40, FieldSet(emptyDataMap(CustomField).addAll(
      Text       (1, "Description", "desc",     Mandatory,     onlyReqTypes(2, 6, StaticReqType.UseCase), Alive),
      Text       (2, "Notes",       "notes",    Mandatory.Not, notReqTypes(4),                            Alive),
      Text       (3, "Reporter",    "reporter", Mandatory,     onlyReqTypes(5, StaticReqType.UseCase),    Dead),
      Tag        (4, 1.TG,  /* Priority */      Mandatory,     ISubset.All(),                             Alive),
      Tag        (5, 10.TG, /* Status */        Mandatory.Not, notReqTypes(5, 6),                         Alive),
      Implication(6, 2,     /* Major Feature */ Mandatory.Not, notReqTypes(6),                            Alive),
      Tag        (7, 27.TG, /* Released */      Mandatory.Not, ISubset.All(),                             Dead)
    ), Vector(
      Text.Id(1), Implication.Id(6), Tag.Id(4), Text.Id(3),
      StaticField.NormalAltStepTree, StaticField.ExceptionStepTree, StaticField.StepGraph,
      Tag.Id(7), Tag.Id(5), Text.Id(2)
    )))
  }

  lazy val reqs     = RevAnd(40, Requirements.empty)
  lazy val reqCodes = RevAnd(50, ReqCodes(Map.empty))
  lazy val reqData  = RevAnd(60, ReqFieldData(Map.empty, Multimap.empty, ReqFieldData.Implications(Multimap.empty)))

  lazy val project = new Project(customIssueTypes, customReqTypes, fields, tagsR, reqs, reqCodes, reqData)

  lazy val tagTree = project.tags.data.mapValues(_.children)

  // lazy val tagTreeB = BiMultimap(Multimap(tagTree.mapValues(_.toSet)))
}
