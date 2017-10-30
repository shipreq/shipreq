package shipreq.webapp.base.data

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.microlibs.stdlib_ext.StdlibExt._
import scalaz.Equal
import scalaz.std.string.stringInstance
import scalaz.std.stream._
import scalaz.std.vector._
import scalaz.syntax.equal._
import shipreq.base.util.MTrie.Ops
import shipreq.base.util.ScalaExt._
import shipreq.base.util.Util
import shipreq.base.util.univeq._
import shipreq.webapp.base.data.Field.ApplicableReqTypes
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.text.{Grammar, PlainText, Text}
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.validation.{CommonValidation => V, _}
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.base.validation.Implicits._
import shipreq.webapp.base.UiText.FieldNames
import shipreq.webapp.base.WebappConfig
import Uniqueness.Util._

object DataValidators {

  private val genericName: Composite.Stateless[String, String, String] =
    V.mandatoryShortText.toValidator.named(FieldNames.name)

  private val genericDesc: Composite.Stateless[String, Option[String], Option[String]] =
    V.optionalLargeText.named(FieldNames.desc)

  def genericRichText(pt: PlainText.ForProject.NoCtx): Invalidator[Text.AnyOptional] =
    Invalidator.test(
      pt.text(_, Live).length <= WebappConfig.largeTextMaxLength,
      Invalidity("Text too large.")) // english

  // TODO Make vals lazy
  lazy val projectName = V.mandatoryShortText.toValidator.named("Project name")

  // ===================================================================================================================
  object hashRefKey {
    import Grammar.{hashRefKey => G}

    // DD-21: Refkeys must be case-insensitive.
    //        eg. #HELLO should match #Hello
    private implicit val equality = Equal.equal[HashRefKey](_.value equalsIgnoreCase _.value)

    /** Validation state (external data) required to validate HashRefKey uniqueness. */
    // DD-19: Hashtag-like refkeys (groupings, incmp) must be unique.
    //        e.g. can't have both a grouping and an incompletion with refkey #X.
    final case class State(tags: SubState[TagId], customIssues: SubState[CustomIssueTypeId]) {
      val invalidator: Invalidator[HashRefKey] =
        tags.invalidator merge customIssues.invalidator
    }

    final case class SubState[Id: Equal](subject: Option[Id], data: () => TraversableOnce[(Option[Id], HashRefKey)]) {
      def invalidator: Invalidator[HashRefKey] =
        Uniqueness.optionalKeyWithValue(data)(subject)
    }

    // DD-18: Hashtag-like refkeys (groupings, incmp) must match this format: /[A-Za-z0-9][A-Za-z0-9_-=.]*/
    val hashRefKey: Composite.Stateful[State, String, String, HashRefKey] =
      G.tailChars.validator
        .append(G.length.validator)
        .prependCorrector(TextMod.noWhitespace.correctFull)
        .mapInvalidator(i => V.invalidator.nonEmpty.whenValid(G.firstChar.invalidator merge i))
        .toValidator
        .mapValid(HashRefKey.apply)
        .named(FieldNames.hashRefKey)
        .stateful(_ appendInvalidator _.invalidator)
  }

  // ===================================================================================================================
  object reqType {
    import Grammar.{reqTypeMnemonic => G}

    final case class State(subject: Option[CustomReqTypeId], allData: () => TraversableOnce[CustomReqType]) {
      def otherData: Iterator[CustomReqType] =
        excludeOptionalKey(subject, allData())(_.id)

      def mnemonicUniqueness: Invalidator[Mnemonic] =
        Uniqueness.set(otherData.foldLeft(StaticReqType.mnemonics)(_ ++ _.allMnemonics))

      def nameUniqueness: Invalidator[String] =
        Uniqueness.within(otherData.map(_.name))
    }

    val mnemonic: Composite.Stateful[State, String, String, Mnemonic] =
      G.chars.validator
        .append(G.length.validator)
        .prependCorrector(TextMod.upperCase.correctLive)
        .appendCorrector(TextMod.noWhitespace.correctFull)
        .mapInvalidator(V.invalidator.nonEmpty.whenValid)
        .toValidator
        .mapValid(ReqType.Mnemonic)
        .named(FieldNames.mnemonic)
        .stateful(_ appendInvalidator _.mnemonicUniqueness)

    def name: Composite.Stateful[State, String, String, String] =
      genericName.stateful(_ appendInvalidator _.nameUniqueness)

    val all: State => Composite.Validator[
      (String, String, ImplicationRequired),
      (String, String, ImplicationRequired),
      (Mnemonic, String, ImplicationRequired)] =
      s => mnemonic(s).named tuple name(s).named tuple Validator.id[ImplicationRequired]
  }

  // ===================================================================================================================
  object customIssueType {
    type State = hashRefKey.State
    val State = hashRefKey.State

    def key = hashRefKey.hashRefKey

    def desc = genericDesc.lift[State]

    val all: State => Composite.Validator[
      (String, String),
      (String, Option[String]),
      (HashRefKey, Option[String])] =
      s => key(s).named tuple desc(s).named
  }

  // ===================================================================================================================
  object field {
    import Grammar.{fieldRefKey => G}

    final case class State(subject: Option[CustomFieldId], allData: () => TraversableOnce[CustomField]) {
      def otherData: Iterator[CustomField] =
        excludeOptionalKey(subject, allData())(_.id)

      def nameUniqueness: Invalidator[String] =
        Uniqueness.within(otherData.map(_.independentName).filterDefined)

      def keyUniqueness: Invalidator[FieldRefKey] =
        Uniqueness.within(otherData.map(_.keyO).filterDefined)

      def tagIdUniqueness: Invalidator[TagId] =
        Uniqueness.within(otherData.map({
          case f: CustomField.Tag => f.tagId.some
          case _: CustomField.Text
             | _: CustomField.Implication => None
        }).filterDefined)

      def reqTypeIdUniqueness: Invalidator[ReqTypeId] =
        Uniqueness.within(otherData.map({
          case f: CustomField.Implication => f.reqTypeId.some
          case _: CustomField.Text
             | _: CustomField.Tag => None
        }).filterDefined)
    }

    // TODO BR-2: A field-set cannot contain more than 30 fields.

    private def nameNotReserved: Invalidator[String] =
      Invalidator.test(!StaticField.names.contains(_), Invalidity("Already in use by built-in features."))

    val name: Composite.Stateful[State, String, String, String] =
      genericName
        .appendInvalidator(nameNotReserved)
        .stateful(_ appendInvalidator _.nameUniqueness)

    // DD-20: Field refkeys must match this format: /[a-z][a-z0-9_]*/
    val key: Composite.Stateful[State, String, String, FieldRefKey] =
      G.tailChars.validator
        .append(G.length.validator)
        .prependCorrector(TextMod.lowerCase.correctLive)
        .appendCorrector(TextMod.noWhitespace.correctFull)
        .mapInvalidator(i => V.invalidator.nonEmpty.whenValid(G.firstChar.invalidator merge i))
        .toValidator
        .mapValid(FieldRefKey.apply)
        .named(FieldNames.fieldRefKey)
        .stateful(_ appendInvalidator _.keyUniqueness)

    def mandatory = Validator.id[Mandatory]
    def reqTypes  = Validator.id[Field.ApplicableReqTypes]

    val textField: State => Composite.Validator[
      (String, String, Mandatory, ApplicableReqTypes),
      (String, String, Mandatory, ApplicableReqTypes),
      (String, FieldRefKey, Mandatory, ApplicableReqTypes)] =
      s => name(s).named tuple key(s).named tuple mandatory tuple reqTypes

    object tagField {
      val tagId: Composite.Stateful[State, Option[TagId], Option[TagId], TagId] =
        V.auditor.optionDefined[TagId]
        .toValidator
        .named("Tag")
        .stateful(_ appendInvalidator _.tagIdUniqueness)

      val all: State => Composite.Validator[
        (Option[TagId], Mandatory, ApplicableReqTypes),
        (Option[TagId], Mandatory, ApplicableReqTypes),
        (TagId, Mandatory, ApplicableReqTypes)] =
        (s: State) => tagId(s).named tuple mandatory tuple reqTypes
    }

    object implField {
      val reqTypeId: Composite.Stateful[State, Option[ReqTypeId], Option[ReqTypeId], ReqTypeId] =
        V.auditor.optionDefined[ReqTypeId]
        .toValidator
        .named("ReqType")
        .stateful(_ appendInvalidator _.reqTypeIdUniqueness)

      val all: State => Composite.Validator[
        (Option[ReqTypeId], Mandatory, ApplicableReqTypes),
        (Option[ReqTypeId], Mandatory, ApplicableReqTypes),
        (ReqTypeId, Mandatory, ApplicableReqTypes)] =
        (s: State) => reqTypeId(s).named tuple mandatory tuple reqTypes
    }
  }

  // ===================================================================================================================
  object tag {
    import hashRefKey.SubState

    final case class State(subject: Option[TagId], allTagData: () => TraversableOnce[Tag], customIssues: SubState[CustomIssueTypeId]) {
      def hashRefKeyState: hashRefKey.State =
        hashRefKey.State(
          SubState(subject, () => allTagData().toIterator.filter(_.keyO.isDefined).map(t => (t.id.some, t.keyO.get))),
          customIssues)

      def otherTagData: Iterator[Tag] =
        excludeOptionalKey(subject, allTagData())(_.id)

      def nameUniqueness: Invalidator[String] =
        Uniqueness.within(otherTagData.map(_.name))
    }

    def name: Composite.Stateful[State, String, String, String] =
      genericName.stateful(_ appendInvalidator _.nameUniqueness)

    def key: Composite.Stateful[State, String, String, HashRefKey] =
      hashRefKey.hashRefKey.contramap(_.hashRefKeyState)

    def desc: Composite.Stateful[State, String, Option[String], Option[String]] =
      genericDesc.lift[State]

    val tagGroup: State => Composite.Validator[
      (String, MutexChildren, String),
      (String, MutexChildren, Option[String]),
      (String, MutexChildren, Option[String])] =
      s => name(s).named tuple Validator.id[MutexChildren] tuple desc(s).named

    val applicableTag: State => Composite.Validator[
      (String, String, String),
      (String, String, Option[String]),
      (String, HashRefKey, Option[String])] =
      s => name(s).named tuple key(s).named tuple desc(s).named
  }

  // ===================================================================================================================
  object reqCode {
    import Grammar.{reqCode => G}
    import ReqCode._

    val node: Composite.Stateless[String, String, Node] =
      G.tailChars.validator
        .append(G.nodeLength.validator)
        .prependCorrector(TextMod.squashUnderscores.correctLive)
        .prependCorrector(TextMod.lowerCase.correctLive)
        .appendCorrector(TextMod.noWhitespace.correctFull)
        .mapInvalidator(i => V.invalidator.nonEmpty.whenValid(G.firstChar.invalidator merge i))
        .toValidator
        .mapValid(Node.applyFn)
        .named(FieldNames.reqCodeNode)

    UnivEq[Value] // Prove Set[Value] is ok
    final case class State(trie: Trie, currentValues: Set[Value]) {

      def valueUniqueness: Invalidator[Value] =
        Invalidator.test(
          v => currentValues.contains(v) || !trie.lookup(v).exists(_.isActive),
          Uniqueness.notUnique)
    }

    private def maxNodesInValue: Invalidator[Value] =
      Invalidator.test(
        _.length <= G.maxNodes,
        Invalidity(s"Cannot have more than ${G.maxNodes} nodes.")) // english

    private val value: Composite.Stateful[State, Value, Value, Value] =
      maxNodesInValue.toAuditor.toValidator
        .named(FieldNames.reqCode)
        .stateful(_ appendInvalidator _.valueUniqueness)

    private def validateNodes: Invalidator[Value] =
      node.unnamed.auditor
        .toInvalidator
        .contramap[Node](_.value)
        .liftTraverse[Vector]
        .contramap(_.whole)

    val valueAndNodes: Validator[Value, Value, Value] =
      value.stateless.unnamed.mapAuditor(_.appendInvalidator(validateNodes))

    private def valueCorrector: Corrector[String, Stream[String]] =
      Corrector(
        code => {
          val c1 = TextMod.noWhitespace(code)
          val c2 = c1.split('.').map(node.unnamed.corrector.live).mkString(".")
          Util.fixBeforeAfter(c1, c2)(_ endsWith ".", _ + ".")
        },
        G.nodeSeqFormat.stream,
        G.nodeSeqFormat.merge)

    private def stringsToNodes: Auditor[Stream[String], Stream[Node]] =
      Auditor.traverse(node.unnamed.apply)

    private def nodesToValue: Auditor[Stream[Node], Value] =
      V.auditor.optionDefined[Value].contramap(NonEmptyVector option _.toVector)

    private def stringsToValue: Auditor[Stream[String], Value] =
      stringsToNodes.andThen(nodesToValue)

    val code: Composite.Stateful[State, String, Stream[String], Value] =
      value.map(valueCorrector.withAuditor(stringsToValue) andThen _)

    /** Validate a set of ReqCodes. Each code should already be validated. */
    val codeSet: Invalidator[Set[Value]] =
      Invalidator.test(
        _.size <= G.maxCodes,
        Invalidity(s"Cannot have more than ${G.maxCodes} codes.")) // english
  }

}
