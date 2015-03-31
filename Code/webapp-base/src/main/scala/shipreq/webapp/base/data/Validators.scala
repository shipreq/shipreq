package shipreq.webapp.base.data

import scalaz.{NonEmptyList, Equal}
import scalaz.std.string.stringInstance
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.Grammar
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
    val hashRefKeyU =
      Grammar.hashRefKeyChars.rule
        .addRule(Rules lengthInRange Grammar.hashRefKeyLength)
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
      Grammar.reqTypeMnemonicChars.rule
        .addRule(Rules lengthInRange Grammar.reqTypeMnemonicLength)
        .liveCorrect(upperCase.andThen)
        .correct(_ andThen noWhitespace andThen upperCase)
        .constraint(nonEmpty >> _)
        .forField("Mnemonic")
        .map(ReqType.Mnemonic)

    private def mnemonicUniqueness = {
      val static = (none[CustomReqType.Id], StaticReqType.mnemonics)
      Uniqueness.againstSetByKeyO[S, CustomReqType.Id, Mnemonic](
        sr => sr._2,
        sr => static #:: sr._1.map(_.tmap2(_.id.some, _.allMnemonics))
      ).fieldName(FieldNames.mnemonic)
    }
    val mnemonicS = mnemonicU.liftS[S].addValidation(mnemonicUniqueness)

    def nameU = genericName

    private def nameUniqueness =
      Uniqueness.entity[CustomReqType].optk(_.id.some).v(_.name).fieldName(FieldNames.name)

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
  object field {
    type S = (Stream[CustomField], Option[CustomField.Id])

    // TODO BR-2: A field-set cannot contain more than 30 fields.

    def nameU =
      genericName.addValidation(nameStatic)

    private def nameStatic =
      ValidationPartU.test[String](n => !StaticField.names.contains(n),
        VFailure.forField(FieldNames.name, NonEmptyList("can not be used for user-defined fields.")))

    private def nameUniqueness =
      Uniqueness.entity[CustomField].optk(_.id.some).optv(_.independentName).fieldName(FieldNames.name)

    val nameS = nameU.liftS[S].addValidation(nameUniqueness)

    // DD-20: Field refkeys must match this format: /[a-z][a-z0-9_]*/
    val keyU =
      Grammar.fieldRefKeyChars.rule
        .addRule(Rules lengthInRange Grammar.fieldRefKeyLength)
        .correct(_ andThen noWhitespace andThen lowerCase)
        .constraint(c => nonEmpty >> (startsWithAlpha + c))
        .forField(FieldNames.fieldRefKey)
        .map(FieldRefKey.apply)

    private def keyUniqueness =
      Uniqueness.entity[CustomField].optk(_.id.some).optv(_.keyO).fieldName(FieldNames.fieldRefKey)

    val keyS = keyU.liftS[S].addValidation(keyUniqueness)

    val mandatoryS = ValidatorU.nop[Mandatory].liftS[S]
    val reqTypesS  = ValidatorU.nop[Field.ApplicableReqTypes].liftS[S]

    val textField = nameS ⊗ keyS ⊗ mandatoryS ⊗ reqTypesS

    object tagField {
      @inline private def tagIdField = "Tag"

      private def tagIdUniqueness =
        Uniqueness.entity[CustomField].optk(_.id.some).optv {
          case  f: CustomField.Tag => f.tagId.some
          case _                   => None
        }.fieldName(tagIdField)

      val tagIdS = ValidationPartU.requireFromOption[Tag.Id](tagIdField).liftS[S].toValidator
                     .addValidation(tagIdUniqueness)

      val all = tagIdS ⊗ mandatoryS ⊗ reqTypesS
    }

    object implField {
      @inline private def reqTypeIdField = "ReqType"

      private def reqTypeIdUniqueness =
        Uniqueness.entity[CustomField].optk(_.id.some).optv {
          case  f: CustomField.Implication => f.reqTypeId.some
          case _                           => None
        }.fieldName(reqTypeIdField)

      val reqTypeIdS = ValidationPartU.requireFromOption[ReqType.Id](reqTypeIdField).liftS[S].toValidator
                         .addValidation(reqTypeIdUniqueness)

      val all = reqTypeIdS ⊗ mandatoryS ⊗ reqTypesS
    }
  }

  // ===================================================================================================================
  object tag {
    type S = (Stream[Tag], shared.HashRefKeyVS)

    def nameU = genericName
    val nameS = nameU.liftS[S].addValidation(nameUniqueness)
    private def nameUniqueness =
      Uniqueness.entity[Tag].optk(_.id.some).v(_.name).fieldName(FieldNames.name)
        .contramapS[S](r => (r._1, r._2.tagData._1))

    def keyU = shared.hashRefKeyU
    def keyS = shared.hashRefKeyS.contramapS[S](_._2)

    def descU = genericDesc
    def descS = descU.liftS[S]

    val tagGroup = nameS ⊗ ValidatorU.nop[MutexChildren].liftS[S] ⊗ descS
    val applTag  = nameS ⊗ keyS ⊗ descS
  }
}