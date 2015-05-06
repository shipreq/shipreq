package shipreq.webapp.base.data

import scalaz.{NonEmptyList, Equal, Traverse}
import scalaz.std.string.stringInstance
import scalaz.std.stream._
import scalaz.syntax.traverse._
import shipreq.base.util.MTrie.Ops
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Util, NonEmptyVector, UnivEq}
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.text.{Text, PlainText, Grammar}
import shipreq.webapp.base.validation._
import shipreq.webapp.base.AppConsts
import shipreq.webapp.base.TextMod._
import shipreq.webapp.base.UiText.FieldNames
import Constraints._
import GenericValidators._

object Validators {

  val genericName = mandatoryShortText(FieldNames.name)

  val genericDesc = optionalLargeText(FieldNames.desc)

//  val genericRichText =
//    ValidationPart.test[PlainText.ForProject, Text.AnyOptional](
//      { case (pt, InputCorrected(txt)) => pt.format(txt).length <= AppConsts.largeTextMaxLength },
//      VFailure.looseMsg("Text too large.")) // english

  def genericRichText(pt: PlainText.ForProject, txt: Text.AnyOptional): ValidationResult[txt.type] =
    ValidationResult.test[txt.type](
      pt.format(txt).length <= AppConsts.largeTextMaxLength,
      txt, VFailure.looseMsg("Text too large.")) // english

  // ===================================================================================================================
  object shared {
    import Grammar.{hashRefKey => G}

    // DD-18: Hashtag-like refkeys (groupings, incmp) must match this format: /[A-Za-z0-9][A-Za-z0-9_-=.]*/
    val hashRefKeyU =
      G.allChars.rule
        .addRule(G.length.rule)
        .correct(noWhitespace.compose)
        .constraint(c => nonEmpty >> (G.firstChar.constraint + c))
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
    type S = (Stream[CustomReqType], Option[CustomReqTypeId])
    import Grammar.{reqTypeMnemonic => G}

    val mnemonicU =
      G.chars.rule
        .addRule(G.length.rule)
        .liveCorrect(upperCase.andThen)
        .correct(_ andThen noWhitespace)
        .constraint(nonEmpty >> _)
        .forField("Mnemonic") // English
        .map(ReqType.Mnemonic)

    private def mnemonicUniqueness = {
      val static = (none[CustomReqTypeId], StaticReqType.mnemonics)
      Uniqueness.againstSetByKeyO[S, CustomReqTypeId, Mnemonic](
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
    import Grammar.{fieldRefKey => G}

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
      G.allChars.rule
        .addRule(G.length.rule)
        .liveCorrect(lowerCase.andThen)
        .correct(_ andThen noWhitespace)
        .constraint(c => nonEmpty >> (G.firstChar.constraint + c))
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
          case f: CustomField.Tag => f.tagId.some
          case _                  => None
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

      val reqTypeIdS = ValidationPartU.requireFromOption[ReqTypeId](reqTypeIdField).liftS[S].toValidator
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

  // ===================================================================================================================
  object reqCode {
    import Grammar.{reqCode => G}
    import ReqCode._

    val node: ValidatorU[String, String, Node] =
      G.allChars.rule
        .addRule(G.nodeLength.rule)
        .liveCorrect(squashUnderscores andThen lowerCase andThen _)
        .correct(_ andThen noWhitespace)
        .constraint(c => nonEmpty >> (G.firstChar.constraint + c))
        .forField(FieldNames.reqCodeNode)
        .map(Node.applyFn)
    

    UnivEq[Value] // Prove Set[Value] is ok
    case class VS(trie: Trie, currentValues: Set[Value])

    val valueU: ValidatorU[Value, Value, Value] =
      ValidationPartU.test[Value](_.value.length <= G.maxNodes,
        VFailure.looseMsg(s"A code cannot have more than ${G.maxNodes} nodes.") // english
      ).toValidator

    def valueUniqueness =
      ValidationPart.test[VS, Value]({ case (vs, InputCorrected(v)) =>
        vs.currentValues.contains(v) || !vs.trie.lookup(v).exists(_.active.isDefined)
      },
      VFailure.forField1(FieldNames.reqCode, "is already in use.")) // english

    val valueS: Validator[VS, Value, Value, Value] =
      valueU.liftS[VS].addValidation(valueUniqueness)

    val code: Validator[VS, String, Stream[String], Value] = {
      def liveCorrect(code: String): String = {
        val c1 = noWhitespace(code)
        val c2 = c1.split('.').map(node.liveCorrect).mkString(".")
        Util.fixBeforeAfter(c1, c2)(_ endsWith ".", _ + ".")
      }

      def parse = CorrectionPartU.apply3[String, Stream[String]](
        liveCorrect,
        G.nodeSeqFormat.apply,
        _.mkString(G.nodeSeparator.toString))

      def mkValue = ValidationPartU[Stream[String], Value] { i =>
        import scalaz.Validation.FlatMap._
        val r1 = i.value.map(node.correctAndValidateU)
        val r2: ValidationResult[Stream[Node]] = Traverse[Stream].sequence(r1)
        val r3 = r2.flatMap(ns => ValidationResult.option(
          NonEmptyVector option ns.toVector,
          VFailure.forField1(FieldNames.reqCode, "cannot be blank."))) // english
        r3
      }

      ValidatorU(parse, mkValue).liftS[VS] andThen valueS
    }

    /** Validate a set of ReqCodes. Each code should already be validated. */
    val codeSet = ValidationPartU.test[Set[Value]](
      _.value.size <= G.maxCodes,
      VFailure.looseMsg(s"You cannot have more than ${G.maxCodes} codes.") // english
    ).toValidator
  }

}