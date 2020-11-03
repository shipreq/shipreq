package shipreq.webapp.base

import shipreq.base.util._
package object data {

  // ----------------------------------------------------------------------------------------------
  // Data -> ID relationship & access

  trait DataId[D] {
    type I
    def id(d: D): I
    val unapplyData: AnyRef => Option[D]

    final def pairWithId(d: D): (I, D) =
      (id(d), d)

    final def mapById(ds: Iterable[D])(implicit ev: UnivEq[I]): Map[I, D] =
      ds.foldLeft(UnivEq.emptyMap[I, D])(_ + pairWithId(_))

    final def emptyIMap(implicit ev: UnivEq[I]) =
      IMap.empty(id)
  }

  type DataIdAux[D, Id] = DataId[D] {type I = Id}

  trait ObjDataId[O, D, Id] extends DataId[D] {
    override final type I = Id
  }

  // ----------------------------------------------------------------------------------------------
  // Implicits

  abstract class DataObjImplicits {
    @inline implicit final def tcCustomFieldTypeImp = CustomField.Implication.IdAccess
    @inline implicit final def tcCustomFieldTypeTag = CustomField.Tag        .IdAccess
    @inline implicit final def tcCustomFieldTypeTxt = CustomField.Text       .IdAccess
    @inline implicit final def tcCustomFieldType    = CustomField            .IdAccess
    @inline implicit final def tcCustomIssueType    = CustomIssueType        .IdAccess
    @inline implicit final def tcCustomReqType      = CustomReqType          .IdAccess
    @inline implicit final def tcTag                = Tag                    .IdAccess
    @inline implicit final def tcReq                = ReqT                   .IdAccess
    @inline implicit final def tcGenericReq         = GenericReq             .IdAccess
    @inline implicit final def tcUseCase            = UseCase                .IdAccess
    @inline implicit final def tcUseCaseStep        = UseCaseStep            .IdAccess
  }

  object DataImplicits extends DataObjImplicits {
    implicit val tagTreeMMTree: MMTree[TagId, TagTree] = TagTree.TagTreeMMTree

    implicit final class DataAnyExt[D](private val d: D) extends AnyVal {
      @inline def id[I](implicit i: DataIdAux[D, I]): I = i.id(d)
      @inline def id_(implicit i: DataId[D]): i.I = i.id(d)
    }
  }

  // ----------------------------------------------------------------------------------------------
  // Data types and functions

  type ImplicationScope = CustomField.Implication.Id \/ Direction
  object ImplicationScope {
    def dir(s: ImplicationScope): Direction =
      s match {
        case \/-(d) => d
        case -\/(_) => CustomField.Implication.dir
      }
  }

  type HashRefTarget       = ApplicableTag \/ CustomIssueType
  type CustomIssueTypeIMap = IMap[CustomIssueTypeId, CustomIssueType]
  type TagTree             = IMap[TagId, TagInTree]
  type GenericReqIMap      = IMap[GenericReqId, GenericReq]
  type UseCaseIMap         = IMap[UseCaseId, UseCase]

  type Req   = ReqT  [ReqTypeId]
  type ReqId = ReqIdT[ReqTypeId]
  type Pubid = PubidT[ReqTypeId]

  type ReqC   = ReqT  [CustomReqTypeId]
  type ReqIdC = ReqIdT[CustomReqTypeId]
  type PubidC = PubidT[CustomReqTypeId]

  @inline
  @nowarn("cat=unused")
  final def emptyDataMap[O, D, Id](o: O)(implicit O: ObjDataId[O, D, Id], ev: UnivEq[Id]): IMap[Id, D] =
    IMap.empty(O.id)
}