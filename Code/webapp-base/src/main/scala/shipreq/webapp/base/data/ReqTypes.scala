package shipreq.webapp.base.data

import monocle.Lenser
import shipreq.base.util.TaggedTypes._

sealed trait ReqType {
  def mnemonic: ReqType.Mnemonic
  def oldMnemonics: Set[ReqType.Mnemonic]
  def name: String
  def imp: ImplicationRequired

  final def allMnemonics: Set[ReqType.Mnemonic] = oldMnemonics + mnemonic
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

  lazy val staticMnemonics =
    (Set.empty[Mnemonic] /: static)(_ ++ _.allMnemonics)
}

// =====================================================================================================================

final case class CustomReqType(id: CustomReqType.Id,
                               mnemonic: ReqType.Mnemonic,
                               oldMnemonics: Set[ReqType.Mnemonic],
                               name: String,
                               imp: ImplicationRequired,
                               alive: Alive) extends ReqType {
  def fullName = s"${mnemonic.value}: $name"
}

object CustomReqType {
  final case class Id(value: Long) extends TaggedLong

  object IdAccess extends ObjDataId[CustomReqType.type, CustomReqType, Id] {
    override def id(d: CustomReqType) = d.id
    override def mkId(l: Long) = Id(l)
    override def setId(a: CustomReqType, b: Id) = a.copy(id = b)
  }
}

object CustomReqTypeL {
  private[this] def l = Lenser[CustomReqType]
  val name         = l(_.name)
  val mnemonic     = l(_.mnemonic)
  val oldMnemonics = l(_.oldMnemonics)
}