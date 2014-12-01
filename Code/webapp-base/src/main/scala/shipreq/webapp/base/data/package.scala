package shipreq.webapp.base

import shipreq.base.util.TaggedTypes.TaggedLong

package object data {

  trait DataAndId {
    type Data
    type Id <: TaggedLong
  }

  trait IdAccessor[T <: DataAndId] {
    def id(d: T#Data): T#Id
    def setId(d: T#Data, id: T#Id): T#Data
    def mkId(l: Long): T#Id // For testing
  }

  trait DataObjImplicits {
    implicit def tcCustomIncmpType = CustomIncmpType
    implicit def tcCustomReqType = CustomReqType
  }

  object DataImplicits extends Project.Implicits with DataObjImplicits {

    implicit class DataAndIdDataExt[Q <: DataAndId](val d: Q#Data) extends AnyVal {
      def id(implicit i: IdAccessor[Q]): Q#Id = i.id(d)
    }
  }

}