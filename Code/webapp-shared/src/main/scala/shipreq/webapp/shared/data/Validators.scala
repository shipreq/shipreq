package shipreq.webapp.shared.data

import shipreq.webapp.shared.AppConsts._
import shipreq.webapp.shared.TextMod._
import shipreq.webapp.shared.validation._
import Constraints._
import GenericValidators._
import ValidatorPlus.Implicits._

object Validators {

  object reqType {

    val mnemonic = ValidatorPlus(
      CorrectionPart.endo(noWhitespace andThen upperCase),
      ValidationPart.forConstraint("Mnemonic",
        nonEmpty >> (
          lengthInRange(reqTypeMnemonicLength) + whitelistCharsR("A-Z")("may only consist of letters."))
      ).map(ReqType.Mnemonic),
      upperCase andThen regex("[^A-Z]".r, "") andThen truncateToLength(reqTypeMnemonicLength))

    val name = mandatoryShortText("Name").toPlus
  }

}