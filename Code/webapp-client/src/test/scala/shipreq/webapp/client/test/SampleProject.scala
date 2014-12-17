package shipreq.webapp.client.test

import shipreq.prop.util.{Multimap, BiMultimap}
import shipreq.webapp.client.ClientData

object SampleProject {

  val project = {
    import shipreq.webapp.base.data._
    import shipreq.webapp.base.UnsafeTypes._

    val customImplTypes = DataSet[CustomIncmpType](10, List(
      CustomIncmpType(1, "TODO", "Something you need To Do.", Alive),
      CustomIncmpType(2, "TBD", "To Be Decided.", Alive)))

    val customReqTypes = DataSet[CustomReqType](20, List(
      CustomReqType(1, "CO", Set.empty, "Constraint", ImplicationNotRequired, Alive),
      CustomReqType(2, "MF", Set.empty, "Major Feature", ImplicationNotRequired, Alive),
      CustomReqType(3, "FR", Set.empty, "Functional Requirement", ImplicationRequired, Alive),
      CustomReqType(4, "BR", Set.empty, "Business Rule", ImplicationNotRequired, Alive),
      CustomReqType(5, "DD", Set("DA", "DDF"), "Data Definition", ImplicationNotRequired, Dead),
      CustomReqType(6, "SI", Set.empty, "Solution Idea", ImplicationRequired, Dead)))

    val tags = RevAnd(30, TagTree(
      Tag.IdAccess mapById List(
        TagGroup(1, "Priority", None, IsEnumLike, Alive),
        ApplicableTag(2, "High Priority", None, "pri=high", Alive),
        ApplicableTag(3, "Medium Priority", None, "pri=med", Alive),
        ApplicableTag(4, "Low Priority", Some("Nice to have. Stuff that probably won't be implemented."), "pri=low", Alive),
        TagGroup(10, "Status", None, NotEnumLike, Alive),
        ApplicableTag(11, "WIP", None, "wip", Alive),
        ApplicableTag(12, "Deferred", None, "defer", Alive),
        TagGroup(20, "Version", None, NotEnumLike, Alive),
        ApplicableTag(21, "v1.x", None, "v1.x", Alive),
        ApplicableTag(22, "v1.0", None, "v1.0", Alive),
        ApplicableTag(23, "v1.1", None, "v1.1", Alive),
        ApplicableTag(24, "v1.2", None, "v1.2", Alive),
        ApplicableTag(25, "v2.x", None, "v2.x", Alive),
        ApplicableTag(26, "v3.x", None, "v3.x", Dead),
        TagGroup(27, "Released", None, NotEnumLike, Alive)),
      BiMultimap(Multimap.empty[Tag.Id, Set, Tag.Id]
        .addvs(1, Set(2, 3, 4))
        .addvs(10, Set(11, 12))
        .addvs(20, Set(27, 21, 25, 26))
        .addvs(21, Set(22, 23, 24))
        .addvs(27, Set(22, 23))
      )))

    new Project(customImplTypes, customReqTypes, tags)
  }

  def clientData = new ClientData(project)

}
