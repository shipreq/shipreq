package shipreq.webapp.base

import shipreq.webapp.base.data._

package object test {

  // ===================================================================================================================
  // This crap probably shouldn't exist.
  // It's here just to move it out of main (where it definitely shouldn't exist).
  // ===================================================================================================================

  trait TestDataId[D] {
    type I
    def mkId(l: Int): I
    def setId(d: D, id: I): D
  }

  trait TestObjDataId[O, D, Id] extends TestDataId[D] {
    override final type I = Id
  }

  type TestDataIdAux[D, Id] = TestDataId[D] {type I = Id}

  implicit object TagId_T extends TestObjDataId[Tag.type, Tag, TagId] {
    override def mkId(l: Int) = ApplicableTagId(l)
    override def setId(t: Tag, i: TagId) = t match {
        case x: TagGroup      => x.copy(id = TagGroupId     (i.value))
        case x: ApplicableTag => x.copy(id = ApplicableTagId(i.value))
      }
  }

  implicit object ReqId_T extends TestObjDataId[ReqT.type, Req, ReqId] {
    override def mkId(l: Int) = GenericReqId(l)
    override def setId(cf: Req, i: ReqId) = cf match {
        case r: GenericReq => r.copy(id = GenericReqId(i.value))
        case r: UseCase    => r.copy(id = UseCaseId   (i.value))
      }
  }

  implicit object CustomReqTypeId_T extends TestObjDataId[CustomReqType.type, CustomReqType, CustomReqTypeId] {
    override def mkId(l: Int) = CustomReqTypeId(l)
    override def setId(a: CustomReqType, b: CustomReqTypeId) = a.copy(id = b)
  }

  implicit object CustomFieldId_T extends TestObjDataId[CustomField.type, CustomField, CustomFieldId] {
    import CustomField._
    override def mkId(l: Int) = Text.Id(l)
    override def setId(cf: CustomField, i: CustomFieldId) = cf match {
        case f: Text        => f.copy(id = Text       .Id(i.value))
        case f: Tag         => f.copy(id = Tag        .Id(i.value))
        case f: Implication => f.copy(id = Implication.Id(i.value))
      }
  }

  implicit object CustomIssueTypeId_T extends TestObjDataId[CustomIssueType.type, CustomIssueType, CustomIssueTypeId] {
    override def mkId(l: Int) = CustomIssueTypeId(l)
    override def setId(a: CustomIssueType, b: CustomIssueTypeId) = a.copy(id = b)
  }
}
