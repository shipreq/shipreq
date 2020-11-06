package shipreq.webapp.member.project.data

import japgolly.microlibs.stdlib_ext.StdlibExt._
import monocle.Iso
import scalaz.Equal
import scalaz.std.list._
import scalaz.std.string.stringInstance
import scalaz.std.vector._
import shipreq.base.util.MTrie.Ops
import shipreq.base.util.ScalaExt._
import shipreq.base.util._
import shipreq.webapp.base.config.WebappConfig
import shipreq.webapp.base.util.TextMod
import shipreq.webapp.base.validation.lib.Implicits._
import shipreq.webapp.base.validation.lib.Simple._
import shipreq.webapp.base.validation.lib.Uniqueness.Util._
import shipreq.webapp.base.validation.lib.{CommonValidation => V, _}
import shipreq.webapp.member.UiText.FieldNames
import shipreq.webapp.member.project.data.ReqType.Mnemonic
import shipreq.webapp.member.project.text.{Grammar, PlainText, Text}

object DataValidators {

  private lazy val genericDesc: Composite.Stateless[String, Option[String], Option[String]] =
    V.optionalLargeText.named(FieldNames.desc)

  def genericRichText(pt: PlainText.ForProject.NoCtx): Invalidator[Text.AnyOptional] =
    Invalidator.test(
      pt.text(_, Live, Optional).length <= WebappConfig.largeTextMaxLength,
      Invalidity("Text too large.")) // english

  def genericRichTextNonEmpty[T <: Text.Generic](t: T, pt: PlainText.ForProject.NoCtx): Auditor[Option[t.NonEmptyText], t.NonEmptyText] =
    V.auditor.optionDefined[t.NonEmptyText]
      .appendInvalidator(genericRichText(pt).contramap(_.whole))

  lazy val projectName = V.mandatoryShortText.toValidator.named("Project name")

  lazy val applicableReqTypes = Validator.id[ApplicableReqTypes].named(FieldNames.applicableReqTypes)

  lazy val colour: Composite.Stateless[String, String, Option[Colour]] =
    EndoCorrector(live = Colour.liveCorrect, full = Colour.correct)
      .withAuditor(Auditor(Colour.parseOption(_).leftMap(Invalidity.apply)))
      .named(FieldNames.colour)

  private lazy val stringsSeparatedByWhitespaceOrCommas: Iso[String, Vector[String \/ String]] =
    Iso(Util.separateByWhitespaceOrCommas)(_.iterator.map(_.merge).mkString)

  lazy val reqTypeSeqStr: Composite.Stateless[String, Vector[String \/ String], Vector[Mnemonic]] =
    reqType.mnemonic
      .stateless
      .vectorWithGaps[String]
      .map(_.imapInput(stringsSeparatedByWhitespaceOrCommas))

  def reqTypeSeqStr(a: Auditor[Mnemonic, ReqTypeId]): Composite.Stateless[String, Vector[String \/ String], Vector[ReqTypeId]] =
    reqTypeSeqStr.map(_.andThenAuditor(a.vector))

  def reqTypeSeqStr(reqTypes: ReqTypes): Composite.Stateless[String, Vector[String \/ String], Vector[ReqTypeId]] =
    reqTypeSeqStr(reqTypeAuditor(reqTypes))

  def reqTypeAuditor(reqTypes: ReqTypes): Auditor[Mnemonic, ReqTypeId] =
    Auditor((m: Mnemonic) =>
      reqTypes.allByMnemonic.get(m) match {
        case Some(rt) => rt.live match {
          case Live => \/-(rt.reqTypeId)
          case Dead => -\/(Invalidity(s"${m.value} has been deleted."))
        }
        case None => -\/(Invalidity(s"${m.value} is not a valid req type."))
      }
    )

  def implicationAuditor(p: Project, subject: Option[ReqId], initialValues: Set[ReqId], dir: Direction): Auditor[Set[ReqId], SetDiff[ReqId]] =
    Auditor { in =>
      val newValues = subject.foldLeft(in)(_ - _) // Tolerate reflexivity
      val diff = SetDiff.compare(initialValues, newValues)

      val pi = p.content.implications
      var is = pi(dir)
      for (i <- subject)
        is = is.mod(i, diff.apply)

      if (Implications.Graph.cycleDetector.hasCycle(is.m))
        -\/(Invalidity("That would cause a cycle in your implication graph."))
      else
        \/-(diff)
    }

  // ===================================================================================================================
  object hashRefKey {
    import Grammar.{hashRefKey => G}

    // DD-21: Refkeys must be case-insensitive.
    //        eg. #HELLO should match #Hello
    private implicit val equality = Equal.equal[HashRefKey](_.value equalsIgnoreCase _.value)

    // DD-19: Hashtag-like refkeys (groupings, incmp) must be unique.
    //        e.g. can't have both a grouping and an incompletion with refkey #X.
    /** Validation state (external data) required to validate HashRefKey uniqueness. */
    final case class State(tags        : SubState[TagId],
                           customIssues: SubState[CustomIssueTypeId]) {

      val invalidator: Invalidator[HashRefKey] =
        tags.invalidator merge customIssues.invalidator
    }

    final case class SubState[Id: Equal](subject: Option[Id], data: () => IterableOnce[(Option[Id], HashRefKey)]) {
      def invalidator: Invalidator[HashRefKey] =
        Uniqueness.optionalKeyWithValue(data)(subject)
    }

    object SubState {

      def tagIds(subject: Option[TagId], allTagData: () => IterableOnce[Tag]): SubState[TagId] =
        SubState(subject, () => allTagData().iterator.filter(_.keyO.isDefined).map(t => (t.id.some, t.keyO.get)))

      def customIssueTypeIds(subject: Option[CustomIssueTypeId], customIssueTypes: CustomIssueTypeIMap): SubState[CustomIssueTypeId] =
        SubState(subject, () => customIssueTypes.valuesIterator.map(t => Some(t.id) -> t.key))
    }

    // DD-18: Hashtag-like refkeys (groupings, incmp) must match this format: /[A-Za-z0-9][A-Za-z0-9_-=.]*/
    val hashRefKey: Composite.Stateful[State, String, String, HashRefKey] =
      G.midChars.validator
        .append(G.length.validator)
        .prependCorrector(TextMod.noWhitespace.correctFull)
        .mapInvalidator(i => V.invalidator.nonEmpty.whenValid(G.firstChar.invalidator merge i))
        .mapInvalidator(i => V.invalidator.nonEmpty.whenValid(G.lastChar.invalidator merge i))
        .toValidator
        .mapValid(HashRefKey.apply)
        .named(FieldNames.hashRefKey)
        .stateful(_ appendInvalidator _.invalidator)
  }

  // ===================================================================================================================
  object reqType {
    import Grammar.{reqTypeMnemonic => G}

    final case class State(subject: Option[CustomReqTypeId], allData: () => IterableOnce[CustomReqType]) {
      def otherData: Iterator[CustomReqType] =
        excludeOptionalKey(subject, allData())(_.id)

      def mnemonicUniqueness: Invalidator[Mnemonic] =
        Uniqueness.set(otherData.foldLeft(StaticReqType.mnemonics)(_ ++ _.allMnemonics))

      def nameUniqueness: Invalidator[String] =
        Uniqueness.stringIgnoreCase(otherData.map(_.name) ++ StaticReqType.values.iterator.map(_.name))
    }

    object State {
      def fromConfig(subject: Option[CustomReqTypeId], cfg: ProjectConfig): State =
        apply(subject, () => cfg.reqTypes.custom.valuesIterator)
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

    val name: Composite.Stateful[State, String, String, String] = {
      V.endoCorrector.singleLineWhitespace
        .appendValidator(V.invalidator.nonEmpty
          .whenValid(Grammar.reqTypeName.chars.validator
            .append(Grammar.reqTypeName.length.validator))
        )
        .toValidator
        .named(FieldNames.name)
        .stateful(_ appendInvalidator _.nameUniqueness)
    }

    def desc = genericDesc.lift[State]

    val all: State => Composite.Validator[
      (String,   String, String,         Mandatory),
      (String,   String, Option[String], Mandatory),
      (Mnemonic, String, Option[String], Mandatory)] =
      s =>
        mnemonic(s).named tuple
        name    (s).named tuple
        desc    (s).named tuple
        Validator.id[Mandatory]

  }

  // ===================================================================================================================
  object customIssueType {
    type State = hashRefKey.State

    object State {
      def fromConfig(subject: Option[CustomIssueTypeId], config: ProjectConfig): State =
        hashRefKey.State(
          hashRefKey.SubState.tagIds(None, () => config.tags.tree.valuesIterator.map(_.tag)),
          hashRefKey.SubState.customIssueTypeIds(subject, config.customIssueTypes))
    }

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

    final case class State(subject: Option[CustomFieldId], allData: () => IterableOnce[CustomField]) {
      def otherData: Iterator[CustomField] =
        excludeOptionalKey(subject, allData())(_.id)

      def nameUniqueness: Invalidator[String] =
        Uniqueness.stringIgnoreCase(otherData.map(_.independentName).filterDefined)

      def tagIdUniqueness: Invalidator[TagGroupId] =
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

    object State {
      def from(subject: Option[CustomFieldId], cfg: ProjectConfig): State =
        apply(subject, () => cfg.fields.customFields.valuesIterator)
    }

    private def nameNotReserved: Invalidator[String] =
      Invalidator.test(!StaticField.names.contains(_), Invalidity("Already in use by built-in features."))

    val name: Composite.Stateful[State, String, String, String] = {
      V.endoCorrector.singleLineWhitespace
        .appendValidator(V.invalidator.nonEmpty
          .whenValid(Grammar.fieldName.chars.validator
            .append(Grammar.fieldName.length.validator))
        )
        .addInvalidator(nameNotReserved)
        .toValidator
        .named(FieldNames.name)
        .stateful(_ appendInvalidator _.nameUniqueness)
    }

    val impSource: Composite.Stateful[State, Option[ReqTypeId], Option[ReqTypeId], ReqTypeId] =
      V.option[ReqTypeId]
        .named(FieldNames.impFieldSource)
        .stateful(_ appendInvalidator _.reqTypeIdUniqueness)

    val tagGroup: Composite.Stateful[State, Option[TagGroupId], Option[TagGroupId], TagGroupId] =
      V.option[TagGroupId]
        .named(FieldNames.impFieldSource)
        .stateful(_ appendInvalidator _.tagIdUniqueness)
  }

  // ===================================================================================================================
  object tag {
    import hashRefKey.SubState

    final case class State(subject     : Option[TagId],
                           allTagData  : () => IterableOnce[Tag],
                           customIssues: SubState[CustomIssueTypeId],
                           reqTypes    : ReqTypes) {

      def hashRefKeyState: hashRefKey.State =
        hashRefKey.State(
          SubState.tagIds(subject, allTagData),
          customIssues)

      def otherTagData: Iterator[Tag] =
        excludeOptionalKey(subject, allTagData())(_.id)

      def nameUniqueness: Invalidator[String] =
        Uniqueness.within(otherTagData.map(_.name))
    }

    object State {
      def fromConfig(subject: Option[TagId], c: ProjectConfig): State =
        State(
          subject      = subject,
          allTagData   = () => c.tags.tree.valuesIterator.map(_.tag),
          customIssues = SubState.customIssueTypeIds(None, c.customIssueTypes),
          reqTypes     = c.reqTypes
        )
    }

    val name: Composite.Stateful[State, String, String, String] = {
      V.endoCorrector.singleLineWhitespace
        .appendValidator(V.invalidator.nonEmpty
          .whenValid(Grammar.tagGroupName.chars.validator
            .append(Grammar.tagGroupName.length.validator))
        )
        .toValidator
        .named(FieldNames.name)
        .stateful(_ appendInvalidator _.nameUniqueness)
    }

    def key: Composite.Stateful[State, String, String, HashRefKey] =
      hashRefKey.hashRefKey.contramap(_.hashRefKeyState)

    def desc: Composite.Stateful[State, String, Option[String], Option[String]] =
      genericDesc.lift[State]

    def reqTypes(s: State): Composite.Stateless[String, Vector[String \/ String], Set[ReqTypeId]] =
      reqTypeSeqStr(s.reqTypes).map(_.mapValid(_.toSet))

    def applicableReqTypes(s: State): Composite.Stateless[(String, Applicability), (Vector[String \/ String], Applicability), ApplicableReqTypes] =
      reqTypes(s).map(_.tuple(Validator.id[Applicability]).mapValid { case (ids, ap) => ApplicableReqTypes(ap, ids) })

    val tagGroup: State => Composite.Validator[
      (String, Exclusivity, String),
      (String, Exclusivity, Option[String]),
      (String, Exclusivity, Option[String])
    ] = s =>
      name(s).named             tuple
      Validator.id[Exclusivity] tuple
      desc(s).named

    val applicableTag: State => Composite.Validator[
      (String    , String        , String        , (String, Applicability)),
      (String    , Option[String], String        , (Vector[String \/ String], Applicability)),
      (HashRefKey, Option[String], Option[Colour], ApplicableReqTypes)
    ] = s =>
      key               (s).named tuple
      desc              (s).named tuple
      colour               .named tuple
      applicableReqTypes(s).named
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
        .named(SpecialBuiltInField.Code.name)
        .stateful(_ appendInvalidator _.valueUniqueness)

    private def validateNodes: Invalidator[Value] =
      node.unnamed.auditor
        .toInvalidator
        .contramap[Node](_.value)
        .liftTraverse[Vector]
        .contramap(_.whole)

    val valueAndNodes: Validator[Value, Value, Value] =
      value.stateless.unnamed.mapAuditor(_.appendInvalidator(validateNodes))

    private def valueCorrector: Corrector[String, List[String]] =
      Corrector(
        code => {
          val c1 = TextMod.noWhitespace(code)
          val c2 = c1.split('.').map(node.unnamed.corrector.live).mkString(".")
          Util.fixBeforeAfter(c1, c2)(_ endsWith ".", _ + ".")
        },
        G.nodeSeqFormat.list,
        G.nodeSeqFormat.merge)

    private def stringsToNodes: Auditor[List[String], List[Node]] =
      Auditor.traverse(node.unnamed.apply)

    private def nodesToValue: Auditor[List[Node], Value] =
      V.auditor.optionDefined[Value].contramap(NonEmptyVector option _.toVector)

    private def stringsToValue: Auditor[List[String], Value] =
      stringsToNodes.andThen(nodesToValue)

    val code: Composite.Stateful[State, String, List[String], Value] =
      value.map(valueCorrector.withAuditor(stringsToValue) andThen _)

    /** Validate a set of ReqCodes. Each code should already be validated. */
    val codeSet: Invalidator[Set[Value]] =
      Invalidator.test(
        _.size <= G.maxCodes,
        Invalidity(s"Cannot have more than ${G.maxCodes} codes.")) // english
  }

}
