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

  val genericName = mandatoryShortText(FieldNames.name)

  val genericDesc = optionalLargeText(FieldNames.desc)

  // ===================================================================================================================
  object shared {

    // DD-18: Hashtag-like refkeys (groupings, incmp) must match this format: /[A-Za-z0-9][A-Za-z0-9_-=.]*/
    // Must not contain: []{}<>
    val hashRefKeyU =
      Rules.whitelistCharsR( """A-Za-z0-9\._=\-""", "may only consist of letters, numbers, and these symbols: . _ = -")
        .addRule(Rules.lengthInRange(hashRefKeyLength))
        .correct(noWhitespace.compose)
        .constraint(c => nonEmpty >> (startsWithAlphaNumeric + c))
        .forField(FieldNames.hashRefKey)
        .map(HashRefKey.apply)

    object HashRefKeyVS {
      type Data[Id] = (Option[Id], Stream[(Option[Id], HashRefKey)])
    }

    /** Validation state (external data) required to validate HashRefKey uniqueness. */
    case class HashRefKeyVS(tagData        : HashRefKeyVS.Data[Tag.Id],
                            customIssueData: HashRefKeyVS.Data[CustomIssueType.Id])

    // DD-19: Hashtag-like refkeys (groupings, incmp) must be unique.
    //        e.g. can't have both a grouping and an incompletion with refkey #X.
    // DD-21: Refkeys must be case-insensitive.
    //        eg. #HELLO should match #Hello
    private def hashRefKeyUniqueness: ValidationPart[HashRefKeyVS, HashRefKey, HashRefKey] = {
      def vp[I: Equal](f: HashRefKeyVS => HashRefKeyVS.Data[I]) =
        Uniqueness.main[HashRefKeyVS, (Option[I], HashRefKey), Option[I], HashRefKey, HashRefKey](
          f(_)._1, f(_)._2, _._1, _._2, Uniqueness.ignoreO[I], a => a equalsIgnoreCase _.value
        ).fieldName(FieldNames.hashRefKey)

      val v1 = vp(_.customIssueData)
      val v2 = vp(_.tagData)
      v1 compose v2
    }

    val hashRefKeyS = hashRefKeyU.liftS[HashRefKeyVS].addValidation(hashRefKeyUniqueness)
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
      Uniqueness.entity[CustomReqType].applyO(_.id.some, _.name).fieldName(FieldNames.name)

    val nameS = nameU.liftS[S].addValidation(nameUniqueness)

    val all = mnemonicS ⊗ nameS ⊗ ValidatorU.nop[ImplicationRequired].liftS[S]
  }

  // ===================================================================================================================
  object customIssueType {
    type S = shared.HashRefKeyVS

    def keyU = shared.hashRefKeyU
    def keyS = shared.hashRefKeyS

    def descU = genericDesc
    def descS = descU.liftS[S]

    val all = keyS ⊗ descS
  }

  // ===================================================================================================================
  object tag {
    type S = (Stream[Tag], shared.HashRefKeyVS)

    def nameU = genericName
    val nameS = nameU.liftS[S].addValidation(nameUniqueness)
    private def nameUniqueness =
      Uniqueness.entity[Tag].applyO(_.id.some, _.name).fieldName(FieldNames.name)
        .contramapS[S](r => (r._1, r._2.tagData._1))

    def keyU = shared.hashRefKeyU
    def keyS = shared.hashRefKeyS.contramapS[S](_._2)

    def descU = genericDesc
    def descS = descU.liftS[S]

    val tagGroup = nameS ⊗ ValidatorU.nop[MutexChildren].liftS[S] ⊗ descS
    val applTag  = nameS ⊗ keyS ⊗ descS
  }
}