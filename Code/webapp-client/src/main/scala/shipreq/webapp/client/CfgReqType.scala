package shipreq.webapp.client

import shipreq.webapp.client.ui.Validator
import shipreq.webapp.shared.data._
import shipreq.webapp.client.ui.table._
import scalaz.{\/-, -\/}
import ReqType.Mnemonic

object CfgReqType {

  type P = CustReqType
  type D = CustReqType.Id

  object MnemonicValidator extends Validator[String, String, Mnemonic] {
    override def liveCorrect = _.toUpperCase().replaceAll("[^A-Z]+","").replaceAll("^(.{6}).+","$1")
    override def correct = _.trim.toUpperCase()
    override def validate = {
      case "" => -\/("It's blank!")
      case s if !s.matches("^[A-Z]{1,6}$") => -\/("A-Z only, 1-6 letters.")
      case s => \/-(Mnemonic(s))
    }
    override def c2i = identity
  }

  object ReqNameValidator extends Validator[String, String, String] {
    override def liveCorrect = identity
    override def correct = _.trim
    override def validate = {
      case "" => -\/("It's blank!")
      case s => \/-(s)
    }
    override def c2i = identity
  }

  //  val x = TableSpecBuilder[P](
//            FieldSpec[P](_.mnemonic)
//  )
}
