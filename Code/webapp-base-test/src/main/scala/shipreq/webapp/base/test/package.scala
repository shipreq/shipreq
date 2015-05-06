package shipreq.webapp.base

import shipreq.webapp.base.data._

package object test {

  // ===================================================================================================================
  // This crap probably shouldn't exist.
  // It's here just to move it out of main (where it definitely shouldn't exist).
  // ===================================================================================================================

  trait TestDataId[D] {
    type I
    def mkId(l: Long): I
    def setId(d: D, id: I): D
  }

  trait TestObjDataId[O, D, Id] extends TestDataId[D] {
    override final type I = Id
  }

  type TestDataIdAux[D, Id] = TestDataId[D] {type I = Id}

  implicit object TagIdT extends TestObjDataId[Tag.type, Tag, TagId] {
    override def mkId(l: Long) = ApplicableTagId(l)
    override def setId(t: Tag, i: TagId) = t match {
        case x: TagGroup      => x.copy(id = TagGroupId     (i.value))
        case x: ApplicableTag => x.copy(id = ApplicableTagId(i.value))
      }
  }

  implicit object ReqIdT extends TestObjDataId[Req.type, Req, ReqId] {
    override def mkId(l: Long) = GenericReqId(l)
    override def setId(cf: Req, i: ReqId) = cf match {
        case r: GenericReq => r.copy(id = GenericReqId(i.value))
      }
  }

  implicit object CustomReqTypeIdT extends TestObjDataId[CustomReqType.type, CustomReqType, CustomReqTypeId] {
    override def mkId(l: Long) = CustomReqTypeId(l)
    override def setId(a: CustomReqType, b: CustomReqTypeId) = a.copy(id = b)
  }

  implicit object CustomFieldIdT extends TestObjDataId[CustomField.type, CustomField, CustomField.Id] {
    import CustomField._
    override def mkId(l: Long) = Text.Id(l)
    override def setId(cf: CustomField, i: Id) = cf match {
        case f: Text        => f.copy(id = Text       .Id(i.value))
        case f: Tag         => f.copy(id = Tag        .Id(i.value))
        case f: Implication => f.copy(id = Implication.Id(i.value))
      }
  }

  implicit object CustomIssueTypeIdT extends TestObjDataId[CustomIssueType.type, CustomIssueType, CustomIssueType.Id] {
    override def mkId(l: Long) = CustomIssueType.Id(l)
    override def setId(a: CustomIssueType, b: CustomIssueType.Id) = a.copy(id = b)
  }
}
