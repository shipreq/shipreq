package shipreq.webapp.util

import net.liftweb.util.Props
import net.liftweb.common.{Empty, Box, Failure, Full}
import shipreq.base.util.{Error, ErrorOr}
import shipreq.base.util.ExternalValueReader.Retriever

object PropsRetrievers {
  private implicit def unbox[T](b: Box[T]): Option[ErrorOr[T]] = b match {
    case Full(v)          => Some(ErrorOr(v))
    case Empty            => None
    case Failure(e, _, _) => Some(Error(e))
  }
  implicit val retrieverS = Retriever[String] (Props.get    (_))
  implicit val retrieverI = Retriever[Int]    (Props.getInt (_))
  implicit val retrieverL = Retriever[Long]   (Props.getLong(_))
  implicit val retrieverB = Retriever[Boolean](Props.getBool(_))
}
