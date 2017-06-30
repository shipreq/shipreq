package shipreq.webapp.base.protocol

import boopickle._
import java.time.Instant
import shipreq.base.util._
import BinCodecGeneric._
import BoopickleMacros._

object BinCodecBaseData {

  implicit lazy val pickleInstant: Pickler[Instant] =
    xmap(Instant.ofEpochMilli)(_.toEpochMilli)

  implicit lazy val pickleVectorTreeLoc: Pickler[VectorTree.Location] =
    pickleNEV

  implicit lazy val pickleVectorTreeParentLoc: Pickler[VectorTree.ParentLocation] =
    implicitly[Pickler[Vector[Int]]].imap(VectorTree.ParentLocation.isoVector)

  implicit lazy val pickleDirection: Pickler[Direction] =
    pickleBool(Forwards)


}
