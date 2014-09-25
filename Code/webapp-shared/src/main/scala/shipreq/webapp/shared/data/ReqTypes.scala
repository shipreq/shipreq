package shipreq.webapp.shared.data

import scalaz.Isomorphism.<=>
import shipreq.base.util.TaggedTypes._

sealed trait ImplicationRequired
case object ImplicationRequired extends ImplicationRequired with (Boolean <=> ImplicationRequired) {
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

  sealed trait Static extends ReqType

  case object UseCase extends Static {
    override def mnemonic     = Mnemonic("UC")
    override def oldMnemonics = Set.empty
    override def name         = "Use Case"
    override def imp          = ImplicationNotRequired
  }

  val static: List[Static] = List(UseCase)
  @inline final def staticG: List[ReqType] = static
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

// TODO ProjectReqTypes copied from Project
final case class ProjectReqTypes(customReqTypes: Map[CustReqType.Id, CustReqType]) {

  lazy val allReqTypes: List[ReqType] =
    customReqTypes.values.foldLeft(ReqType.static ::: List.empty[ReqType])((a, b) => b :: a)

  // constraints:
  // - Σ oldMnemonics + Σ mnemonic contains no dups

  lazy val allActiveMnemonics: Set[ReqType.Mnemonic] =
    allReqTypes.foldLeft(Set.empty[ReqType.Mnemonic])((a, r) => a + r.mnemonic)

  lazy val allMnemonics: Set[ReqType.Mnemonic] =
    allReqTypes.foldLeft(allActiveMnemonics)((a, r) => a ++ r.oldMnemonics)
}