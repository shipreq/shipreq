package shipreq.webapp.shared.protocol

import upickle.key
import shipreq.webapp.shared.data._
import Codecs._
import Routine._

object Routines {

  object Square extends DescT[Int, String]
  object Half extends DescT[Int, String]
  object Grrr extends DescT[ExampleData, ExampleData]

  object CustReqTypeUpdate extends DescT[
    (CustReqType.Id, ReqType.Mnemonic, String, ImplicationRequired),
    Option[CustReqType]]

  case class WIP(@key("a") square: Square.Remote,
                 @key("b") half: Half.Remote,
                 @key("c") grrr: Grrr.Remote) extends Group

}

case class ExampleData(i: Int) {
  def yar = s"yar → $i"
}