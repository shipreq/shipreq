package shipreq.webapp.shared.data

import shipreq.base.prop._

trait DataSetAccessor[DI <: DataAndId] {
  def getRev(p: Project): Rev
  def getData(p: Project): Stream[DI#Data]
  def set(p: Project, r: Rev, d: Stream[DI#Data]): Project
}

case class DataSet[DI <: DataAndId](rev: Rev, data: List[DI#Data])

object Project {
  private def DSA[DI <: DataAndId](I: IdAccessor[DI])(// TODO this is just a lens...
                                   getDataSet: Project => DataSet[DI],
                                   setDataSet: (Project, DataSet[DI]) => Project)
                                  : DataSetAccessor[DI] =
    new DataSetAccessor[DI] {
      override def getRev(p: Project) = getDataSet(p).rev
      override def getData(p: Project) = getDataSet(p).data.toStream
      override def set(p: Project, r: Rev, d: Stream[DI#Data]) = setDataSet(p, DataSet[DI](r, d.toList))
    }

  trait Implicits {
    implicit val dsaCustomIncmpType = DSA(CustomIncmpType)(_.customIncmpTypes, (a, b) ⇒ a.copy(customIncmpTypes = b))
    implicit val dsaCustomReqType   = DSA(CustomReqType)  (_.customReqTypes,   (a, b) ⇒ a.copy(customReqTypes = b))
  }
}

final case class Project(customIncmpTypes: DataSet[CustomIncmpTypeAndId],
                         customReqTypes:   DataSet[CustomReqTypeAndId]) {

  def rev = customIncmpTypes.rev + customReqTypes.rev
  this assertSatisfies DataProp.project
}
