package shipreq.webapp.shared.data

import scalaz.Equal
import scalaz.Isomorphism.<=>
import shipreq.base.util.TaggedTypes._

sealed trait ImplicationRequired
case object ImplicationRequired extends ImplicationRequired with (Boolean <=> ImplicationRequired) {
  implicit val equal = Equal.equalA[ImplicationRequired]
  override def from = _ == ImplicationRequired
  override def to = b => if (b) ImplicationRequired else ImplicationNotRequired
}
case object ImplicationNotRequired extends ImplicationRequired

sealed trait ReqType {
  def mnemonic: ReqType.Mnemonic
  def oldMnemonics: Set[ReqType.Mnemonic]
  def name: String
  def imp: ImplicationRequired
}

object ReqType {
  final case class Mnemonic(value: String) extends TaggedString

  implicit val scalaOrdering = Ordering.by((_: ReqType.Mnemonic).value)

  sealed trait Static extends ReqType

  case object UseCase extends Static {
    override def mnemonic     = Mnemonic("UC")
    override def oldMnemonics = Set.empty
    override def name         = "Use Case"
    override def imp          = ImplicationNotRequired
  }

  val static: List[Static] = List(UseCase)
}

// constraints:
// - oldMnemonics doesn't include mnemonic
final case class CustomReqType(id: CustomReqType.Id,
                               mnemonic: ReqType.Mnemonic,
                               oldMnemonics: Set[ReqType.Mnemonic],
                               name: String,
                               imp: ImplicationRequired,
                               alive: Alive) extends ReqType

object CustomReqType {
  final case class Id(value: Long) extends TaggedLong
}
