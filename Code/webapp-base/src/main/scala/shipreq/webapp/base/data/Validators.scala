package shipreq.webapp.base.data

import scalaz.Equal
import scalaz.std.string.stringInstance
import scalaz.syntax.equal.ToEqualOps
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.AppConsts._
import shipreq.webapp.base.TextMod._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.validation._
import Constraints._
import GenericValidators._

object Validators {

  val genericName = mandatoryShortText("Name")

  val genericDesc = optionalLargeText(FieldNames.desc)

  // ===================================================================================================================
  object shared {

    // DD-18: Hashtag-like refkeys (groupings, incmp) must match this format: /[A-Za-z0-9][A-Za-z0-9_-=.]*/
    // Must not contain: []{}<>
    // TODO should refkey uniqueness and matching be case-insensitive? Probably.
    val refKeyU =
      Rules.whitelistCharsR( """A-Za-z0-9\._=\-""", "may only consist of letters, numbers, and these symbols: . _ = -")
        .addRule(Rules.lengthInRange(refKeyLength))
        .correct(noWhitespace.compose)
        .constraint(c => nonEmpty >> (startsWithAlphaNumeric + c))
        .forField(FieldNames.refKey)
        .map(RefKey.apply)

    object RefKeyVS {
      type Data[Id] = (Option[Id], Stream[(Option[Id], RefKey)])
    }

    /** Validation state (external data) required to validate refkey uniqueness. */
    case class RefKeyVS(tagData: RefKeyVS.Data[Tag.Id],
                        incmpData: RefKeyVS.Data[CustomIncmpType.Id])

    private def refKeyUniqueness: ValidationPart[RefKeyVS, RefKey, RefKey] = {
      def vp[I: Equal](f: RefKeyVS => RefKeyVS.Data[I]) =
        Uniqueness.main[RefKeyVS, (Option[I], RefKey), Option[I], RefKey, RefKey](
          f(_)._1, f(_)._2, _._1, _._2, Uniqueness.ignoreO[I], _.≟).fieldName(FieldNames.refKey)
      val v1 = vp(_.incmpData)
      val v2 = vp(_.tagData)
      v1 compose v2
    }

    val refKeyS = refKeyU.liftS[RefKeyVS].addValidation(refKeyUniqueness)
  }

  // ===================================================================================================================
  object reqType {
    type S = (Stream[CustomReqType], Option[CustomReqType.Id])

    val mnemonicU =
      Rules.whitelistCharsR("A-Z", "may only consist of letters.")
        .addRule(Rules.lengthInRange(reqTypeMnemonicLength))
        .liveCorrect(upperCase.andThen)
        .correct(_ andThen noWhitespace andThen upperCase)
        .constraint(nonEmpty >> _)
        .forField("Mnemonic")
        .map(ReqType.Mnemonic)

    private def mnemonicUniqueness = {
      val static = (none[CustomReqType.Id],  ReqType.staticMnemonics)
      Uniqueness.againstSetByKeyO[S, CustomReqType.Id, Mnemonic](
        sr => sr._2,
        sr => static #:: sr._1.map(_.tmap2(_.id.some, _.allMnemonics))
      ).fieldName(FieldNames.mnemonic)
    }
    val mnemonicS = mnemonicU.liftS[S].addValidation(mnemonicUniqueness)

    def nameU = genericName

    private def nameUniqueness =
      Uniqueness.entity[CustomReqType].applyO(_.id.some, _.name).fieldName("Name")

    val nameS = nameU.liftS[S].addValidation(nameUniqueness)

    val all = mnemonicS ⊗ nameS ⊗ ValidatorU.nop[ImplicationRequired].liftS[S]
  }

  // ===================================================================================================================
  object customIncmpType {
    type S = shared.RefKeyVS

    def keyU = shared.refKeyU
    def keyS = shared.refKeyS

    def descU = genericDesc
    def descS = descU.liftS[S]

    val all = keyS ⊗ descS
  }

  // ===================================================================================================================
  object tag {
    type S = (Stream[Tag], Option[Tag.Id])

    def nameU = genericName

    private def nameUniqueness =
      Uniqueness.entity[Tag].applyO(_.id.some, _.name).fieldName("Name")

    val nameS = nameU.liftS[S].addValidation(nameUniqueness)

//    def keyU  = refKey

    def descU = genericDesc

    // refkey - unique
    // no cycles
  }
}