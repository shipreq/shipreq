package shipreq.webapp.base

import shipreq.base.util.IMap

package object data {

  trait DataId[D] {
    type I
    def id(d: D): I
    def setId(d: D, id: I): D
    def mkId(l: Long): I // For testing

    final def pairWithId(d: D): (I, D) = (id(d), d)
    final def mapById(ds: Traversable[D]): Map[I, D] = (Map.empty[I, D] /: ds)(_ + pairWithId(_))
  }

  type DataIdAux[D, Id] = DataId[D] {type I = Id}

  trait ObjDataId[O, D, Id] extends DataId[D] {
    override final type I = Id
  }

  abstract class DataObjImplicits {
    @inline implicit final def tcCustomIncmpType = CustomIncmpType.IdAccess
    @inline implicit final def tcCustomReqType   = CustomReqType.IdAccess
    @inline implicit final def tcTag             = Tag.IdAccess
    @inline implicit final def tcTagPov          = TagProtocol.PovTag.IdAccess
  }

  object DataImplicits extends DataObjImplicits with Project.Implicits {

    implicit final class DataAnyExt[D](val d: D) extends AnyVal {
      @inline def id[I](implicit i: DataIdAux[D, I]): I = i.id(d)
      @inline def id_(implicit i: DataId[D]): i.I = i.id(d)
    }
  }

  type TagTree = IMap[Tag.Id, TagInTree]

}