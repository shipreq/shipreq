package shipreq.taskman.server

import scalaz.std.option._
import scalaz.syntax.traverse._
import shipreq.base.util.ExternalValueReader.Retriever
import shipreq.base.util.{StringBasedValueReader, ErrorOr}
import shipreq.taskman.server.Sop.CfgGet

object CfgValueReader {

  def apply(sopReifier: SopReifier) =
    new StringBasedValueReader(
      new Retriever[String](
        k => ErrorOr.safe(sopReifier(CfgGet(k)).unsafePerformIO()).sequence))

}
