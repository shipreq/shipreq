package shipreq.webapp.base.test

import shipreq.prop.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.UnsafeTypes._

object SampleProject {

  val customImplTypes = List(
    CustomIncmpType(1, "TODO", "Something you need To Do.", Alive),
    CustomIncmpType(2, "TBD", "To Be Decided.", Alive))

  val customReqTypes = List(
    CustomReqType(1, "CO", Set.empty, "Constraint", ImplicationNotRequired, Alive),
    CustomReqType(2, "MF", Set.empty, "Major Feature", ImplicationNotRequired, Alive),
    CustomReqType(3, "FR", Set.empty, "Functional Requirement", ImplicationRequired, Alive),
    CustomReqType(4, "BR", Set.empty, "Business Rule", ImplicationNotRequired, Alive),
    CustomReqType(5, "DD", Set("DA", "DDF"), "Data Definition", ImplicationNotRequired, Dead),
    CustomReqType(6, "SI", Set.empty, "Solution Idea", ImplicationRequired, Dead))

  val v10d = Some("Released: 17/14/1976\nFirst release.")
  val v11d = Some("Released: 1/2/2001")
  val tags = TagTree.empty.addAll(
    TagInTree(TagGroup     (1, "Priority",        None, IsEnumLike,  Alive), Vector(2,3,4)),
    TagInTree(ApplicableTag(2, "High Priority",   None, "pri=high",  Alive), Vector()),
    TagInTree(ApplicableTag(3, "Medium Priority", None, "pri=med",   Alive), Vector()),
    TagInTree(TagGroup     (10, "Status",         None, NotEnumLike, Alive), Vector(11,12)),
    TagInTree(ApplicableTag(11, "WIP",            None, "wip",       Alive), Vector()),
    TagInTree(ApplicableTag(12, "Deferred",       None, "defer",     Alive), Vector()),
    TagInTree(TagGroup     (20, "Version",        None, NotEnumLike, Alive), Vector(27,21,25,26)),
    TagInTree(ApplicableTag(21, "v1.x",           None, "v1.x",      Alive), Vector(22,23,24)),
    TagInTree(ApplicableTag(22, "v1.0",           v10d, "v1.0",      Alive), Vector()),
    TagInTree(ApplicableTag(23, "v1.1",           v11d, "v1.1",      Alive), Vector()),
    TagInTree(ApplicableTag(24, "v1.2",           None, "v1.2",      Alive), Vector()),
    TagInTree(ApplicableTag(25, "v2.x",           None, "v2.x",      Alive), Vector()),
    TagInTree(ApplicableTag(26, "v3.x",           None, "v3.x",      Dead ), Vector()),
    TagInTree(TagGroup     (27, "Released",       None, NotEnumLike, Alive), Vector(22,23)),
    TagInTree(ApplicableTag(4, "Low Priority", Some("Nice to have. Stuff that probably won't be implemented."), "pri=low", Alive), Vector()))

  val project = new Project(
    DataSet[CustomIncmpType](10, customImplTypes),
    DataSet[CustomReqType](20, customReqTypes),
    RevAnd(30, tags))

  val tagTree = project.tags.data.mapValues(_.children)

  // val tagTreeB = BiMultimap(Multimap(tagTree.mapValues(_.toSet)))
}
