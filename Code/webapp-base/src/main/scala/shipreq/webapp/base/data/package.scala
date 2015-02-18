package shipreq.webapp.base

import shipreq.base.util.IMap

package object data {

  // ----------------------------------------------------------------------------------------------
  // Data → ID relationship & access

  trait DataId[D] {
    type I
    def id(d: D): I
    val unapplyData: AnyRef => Option[D]

    def mkId(l: Long): I // For testing

    final def pairWithId(d: D): (I, D) = (id(d), d)
    final def mapById(ds: Traversable[D]): Map[I, D] = (Map.empty[I, D] /: ds)(_ + pairWithId(_))
    final def emptyIMap = IMap.empty(id)
  }

  type DataIdAux[D, Id] = DataId[D] {type I = Id}

  trait ObjDataId[O, D, Id] extends DataId[D] {
    override final type I = Id
  }

  // ----------------------------------------------------------------------------------------------
  // Data ID modification

  trait DataIdM[D] extends DataId[D] {
    def setId(d: D, id: I): D
  }

  trait ObjDataIdM[O, D, Id] extends ObjDataId[O, D, Id] with DataIdM[D]

  type DataIdMAux[D, Id] = DataIdM[D] {type I = Id}

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
    @inline implicit final def tcReq                = Req                    .IdAccess
  }

  object DataImplicits extends DataObjImplicits {

    implicit final class DataAnyExt[D](val d: D) extends AnyVal {
      @inline def id[I](implicit i: DataIdAux[D, I]): I = i.id(d)
      @inline def id_(implicit i: DataId[D]): i.I = i.id(d)
    }
  }

  // ----------------------------------------------------------------------------------------------
  // Data types and functions

  type CustomIssueTypeIMap = IMap[CustomIssueType.Id, CustomIssueType]
  type CustomReqTypeIMap   = IMap[CustomReqType.Id, CustomReqType]
  type TagTree             = IMap[Tag.Id, TagInTree]

  @inline final def emptyDataMap[O, D, Id](o: O)(implicit O: ObjDataId[O, D, Id]): IMap[Id, D] =
    IMap.empty(O.id)
}