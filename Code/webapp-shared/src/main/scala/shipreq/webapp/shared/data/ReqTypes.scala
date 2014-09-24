package shipreq.webapp.shared.data

import shipreq.base.util.TaggedTypes._

sealed trait ImplicationRequired
case object ImplicationRequired extends ImplicationRequired
case object ImplicationNotRequired extends ImplicationRequired

sealed trait ReqType {
  def mnemonic: ReqType.Mnemonic
  def oldMnemonics: Set[ReqType.Mnemonic]
  def name: String
}

object ReqType {
  final case class Mnemonic(value: String) extends TaggedString

  case object UseCase extends ReqType {
    override def mnemonic     = Mnemonic("UC")
    override def oldMnemonics = Set.empty
    override def name         = "Use Case"
  }

  val static: List[ReqType] = List(UseCase)
}

// constraints:
// - oldMnemonics doesn't include mnemonic
final case class CustReqType(id: CustReqType.Id,
                             mnemonic: ReqType.Mnemonic,
                             oldMnemonics: Set[ReqType.Mnemonic],
                             name: String,
                             imp: ImplicationRequired,
                             alive: Alive) extends ReqType

object CustReqType {
  final case class Id(value: Long) extends TaggedLong
}
