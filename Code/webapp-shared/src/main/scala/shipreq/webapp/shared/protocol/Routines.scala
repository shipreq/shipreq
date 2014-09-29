package shipreq.webapp.shared.protocol

import Routine._
import upickle.key

object Routines {

  object Square extends DescT[Int, String]
  object Half extends DescT[Int, String]
  object Grrr extends DescT[ExampleData, ExampleData]

  case class WIP(@key("a") square: Square.Remote,
                 @key("b") half: Half.Remote,
                 @key("c") grrr: Grrr.Remote) extends Group

}

case class ExampleData(i: Int) {
  def yar = s"yar → $i"
}