package shipreq.webapp.client.project.app.pages.config.fields

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util.Util.ShipReqOpsForArraySeq
import shipreq.base.util._
import shipreq.webapp.base.ui.semantic.UsesSemanticUiManually
import shipreq.webapp.client.project.app.Style.{fieldConfig => *}
import shipreq.webapp.client.project.util.DataReusability._
import shipreq.webapp.member.feature.AutoCompleteFeature._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.derivation.TagGroupTags
import shipreq.webapp.member.ui.AutosizeTextarea

private[fields] object DerivativeTagRuleEditor {
  import DerivativeTags.TagPair

  final case class Props(state: StateSnapshot[State],
                         group: TagGroupId,
                         tags: Tags) {
    @inline def render: VdomElement = Component(this)
  }

  // ===================================================================================================================

  final case class State(initialValue: DerivativeTags.Rules,
                         initialScope: State.Scope,
                         ir          : IR) {

    def onTextChange(text: String, group: TagGroupId, tags: Tags): State = {
      val newIR = IR.fromText(text, group, tags)
      copy(ir = newIR)
    }

    lazy val irLines: ArraySeq[IR] =
      ir.splitOn(IR.Atom.NewLine)

    def validate(groupTags: FilterDead.Values[TagGroupTags]): Validated =
      Validated.derive(this, groupTags)

    def potentialChange(validated: Validated): PotentialChange[Unit, DerivativeTags.Rules] =
      if (validated.invalidity.isEmpty) {
        def hidden = initialValue.iterator.filter(kv => !initialScope.contains(kv._1))
        val newRules = validated.validRules ++ hidden
        PotentialChange.Success(newRules).ignoreValue(initialValue)
      } else
        PotentialChange.Failure(())
  }

  object State {

    /** Dead or invalid tags will be removed from scope and hidden from the editor. */
    type Scope = Set[TagPair]

    def empty: State =
      State(Map.empty, Set.empty, IR.empty)

    def init(dt: DerivativeTags, group: TagGroupId, tags: Tags): State = {
      val liveTags = tags.tagGroupTags(group)(HideDead)
      val inScope  = dt.rules.keysIterator.filter(_.forAll(liveTags.contains)).toSet
      val ir       = IR.fromDerivativeTags(dt, inScope, tags)
      State(dt.rules, inScope, ir)
    }
  }

  // ===================================================================================================================

  type IR = ArraySeq[IR.Atom]

  object IR {
    def empty: IR =
      ArraySeq.empty

    sealed trait Atom
    object Atom {
      case object      Plus                        extends Atom
      case object      Equals                      extends Atom
      case object      NewLine                     extends Atom
      final case class Tag(value: ApplicableTagId) extends Atom
      final case class Word(value: String)         extends Atom
      final case class Space(size: Int)            extends Atom

      implicit def univEq: UnivEq[Atom] = UnivEq.derive
      implicit val reusability: Reusability[Atom] = Reusability.byRefOrUnivEq
    }

    def fromDerivativeTags(deriv: DerivativeTags, scope: State.Scope, allTags: Tags): IR = {
      var inScope = List.empty[(ApplicableTagId, String, ApplicableTagId, String, ApplicableTagId)]
      val alignL = Aligner.forStrings()
      val alignR = Aligner.forStrings()

      // First pass: filter, order conjuncts, measure conjunct lens
      for ((k, v) <- deriv.rules) {
        if (scope.contains(k)) {
          var lo = k.lo
          var hi = k.hi
          var loName = allTags.needApplicableTag(lo).name
          var hiName = allTags.needApplicableTag(hi).name
          if (loName.compareTo(hiName) > 0) {
            lo = k.hi
            hi = k.lo
            val x = loName
            loName = hiName
            hiName = x
          }
          inScope ::= ((lo, loName, hi, hiName, v))
          alignL.consider(loName)
          alignR.consider(hiName)
        }
      }

      // Second pass: order rows
      val ordered = MutableArray(inScope).sortBySchwartzian(x => (x._2, x._4)).arraySeq

      // Third pass: build IR
      val result = ArraySeq.newBuilder[Atom]
      val oneSpace = Atom.Space(1)
      ordered.foreach { case (a, na, b, nb, c) =>
        result += Atom.Tag(a)
        result += Atom.Space(alignL.paddingSize(na.length) + 1)
        result += Atom.Plus
        result += oneSpace
        result += Atom.Tag(b)
        result += Atom.Space(alignR.paddingSize(nb.length) + 1)
        result += Atom.Equals
        result += oneSpace
        result += Atom.Tag(c)
        result += Atom.NewLine
      }
      result.result()
    }

    def fromText(text: String, groupId: TagGroupId, tags: Tags): IR = {
      val groupTags = tags.tagGroupTags(groupId)(ShowDead)
      val tagLookup: String => Option[ApplicableTagId] =
        s =>
          tags.applicableTagLookup(s)
            .orElse(groupTags.abbreviations.getByName(s))
            .map(_.id)

      new TextParser(text, tagLookup).all.run().get.to(ArraySeq)
    }

    def toText(ir: IR, tags: Tags): String = {
      var text = ""
      for (a <- ir) {
        val s: String = a match {
          case Atom.Plus     => "+"
          case Atom.Equals   => "="
          case Atom.NewLine  => "\n"
          case Atom.Tag(id)  => tags.needApplicableTag(id).name
          case Atom.Word(w)  => w
          case Atom.Space(l) => " " * l
        }
        text += s
      }
      text
    }

    // -----------------------------------------------------------------------------------------------------------------
    import org.parboiled2._

    private val wordCP = CharPredicate.All -- "+\n " -- EOI

    private class TextParser(val input: ParserInput, tagLookup: String => Option[ApplicableTagId]) extends Parser {

      private def plus: Rule1[Atom.Plus.type] =
        rule('+' ~ push(Atom.Plus))

      private def equals: Rule1[Atom.Equals.type] =
        rule('=' ~ push(Atom.Equals))

      private def newLine: Rule1[Atom.NewLine.type] =
        rule('\n' ~ push(Atom.NewLine))

      private def tagOrWord: Rule1[Atom] =
        rule(capture(oneOrMore(wordCP)) ~> ((s: String) => tagLookup(s).fold[Atom](Atom.Word(s))(Atom.Tag)))

      private def space: Rule1[Atom.Space] =
        rule(capture(oneOrMore(' ')) ~> ((s: String) => Atom.Space(s.length)))

//      private def fallback: Rule1[Atom] =
//        rule(capture(oneOrMore(!EOI ~ ANY)) ~!~ test(true) ~> { (s: String) =>
//          throw new RuntimeException("[" + s + "]")
//          Atom.NewLine
//        })

      private def atom: Rule1[Atom] =
        rule(plus | equals | newLine | space | tagOrWord) // | fallback

      def all: Rule1[Seq[Atom]] =
        rule(zeroOrMore(atom) ~ EOI)
    }
  }

  // ===================================================================================================================

  sealed trait Warning
  object Warning {
    final case class DeadTarget    (id: ApplicableTagId) extends Warning
    final case class ExplicitRefl  (id: ApplicableTagId) extends Warning
    final case class ExternalTarget(id: ApplicableTagId) extends Warning
    implicit def univEq: UnivEq[Warning] = UnivEq.derive
  }

  sealed trait Invalidity
  object Invalidity {
    final case class BadStatement  (ir: IR)              extends Invalidity
    final case class Conflict      (stmts: Set[IR])      extends Invalidity
    final case class DeadSource    (id: ApplicableTagId) extends Invalidity
    final case class ExternalSource(id: ApplicableTagId) extends Invalidity
    final case class SameSources   (id: ApplicableTagId) extends Invalidity
    final case class TagNotFound   (txt: String)         extends Invalidity
    implicit def univEq: UnivEq[Invalidity] = UnivEq.derive
  }

  final case class Validated(validRules: DerivativeTags.Rules,
                             warnings  : Set[Warning],
                             invalidity: Set[Invalidity])

  object Validated {
    implicit def univEq: UnivEq[Validated] = UnivEq.derive

    private[DerivativeTagRuleEditor] def derive(s: State, groupTags: FilterDead.Values[TagGroupTags]): Validated = {
      import IR.Atom._

      val liveTags   = groupTags(HideDead)
      val deadTags   = groupTags(ShowDead)
      var validStmts = Multimap.empty[TagPair, List, (IR, ApplicableTagId)]
      var warnings   = UnivEq.emptySet[Warning]
      var invalidity = UnivEq.emptySet[Invalidity]

      def validateSource(id: ApplicableTagId): Validity =
        if (liveTags.contains(id))
          Valid
        else if (deadTags.contains(id)) {
          invalidity += Invalidity.DeadSource(id)
          Invalid
        } else {
          invalidity += Invalidity.ExternalSource(id)
          Invalid
        }

      def validateTarget(id: ApplicableTagId): Unit =
        if (liveTags.contains(id))
          ()
        else if (deadTags.contains(id))
          warnings += Warning.DeadTarget(id)
        else
          warnings += Warning.ExternalTarget(id)

      // Process line by line
      for (line <- s.irLines) {

        // Scan line's atoms
        var ok = true
        line.foreach {
          case Word(w) =>
            invalidity += Invalidity.TagNotFound(w)
            ok = false
          case _ =>
        }

        // Parse line as a statement
        if (ok) {
          val lineWithoutSpaces = line.filterNot(_.isInstanceOf[Space])
          lineWithoutSpaces match {

            // a + b = c
            case ArraySeq(Tag(a), Plus, Tag(b), Equals, Tag(c)) =>
              val va = validateSource(a)
              val vb = validateSource(b)
              validateTarget(c)
              if ((va & vb) is Valid) {
                if (a !=* b)
                  validStmts = validStmts.add(TagPair(a, b), (line, c))
                else if (a ==* c)
                  warnings += Warning.ExplicitRefl(a)
                else
                  invalidity += Invalidity.SameSources(a)
              }

            case ArraySeq() =>
              // empty line

            case _ =>
              invalidity += Invalidity.BadStatement(line)
          }
        }
      }

      // Process valid statements
      var validRules: DerivativeTags.Rules = UnivEq.emptyMap
      validStmts.iterator.foreach { case (source, values) =>
        val targets = values.iterator.map(_._2).toSet
        if (targets.sizeIs == 1)
          validRules += source -> targets.head
        else
          invalidity += Invalidity.Conflict(values.iterator.map(_._1).toSet)
      }

      Validated(validRules, warnings, invalidity)
    }
  }

  // ===================================================================================================================

  private[fields] def autoComplete(tags: TagGroupTags): AutoComplete.Strategies = {

    val sorted = MutableArray(tags.tags).map(_.name.toLowerCase).sort.arraySeq

    val searchFn: String => IterableOnce[String] =
      ss => {
        val s = ss.toLowerCase
        sorted.iterator.filter(_.startsWith(s)).take(MaxResults)
      }

    def instance(before: String, suffix: String) =
      AutoComplete.Strategy.builder
        .regex(s"((?:$before) *)([^\\s=+]*)$$", index = 2)
        .search(searchFn)
        .replace("$1" + _ + suffix)
        .result()

    Vector(
      instance("^|\\n", " +"),
      instance("\\+", " ="),
      instance("=", "\n"),
    )
  }

  @UsesSemanticUiManually
  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.BackendTA {

    private val pxGroup: Px[TagGroupId] =
      Px.props($).map(_.group).withReuse.autoRefresh

    private val pxTags: Px[Tags] =
      Px.props($).map(_.tags).withReuse.autoRefresh

    private val pxAutoComplete: Px[AutoComplete.Strategies] =
      for {
        group <- pxGroup
        tags  <- pxTags
      } yield {
        val groupTags = tags.tagGroupTags(group)(HideDead)
        autoComplete(groupTags)
      }

    private val editorRef =
      Ref.toScalaComponent(AutosizeTextarea.Component)

    override val autoCompleteCtx: CallbackOption[AutoCompleteCtx] =
      for {
        r <- editorRef.get.asCBO
        h <- CallbackOption.option(r.getDOMNode.toHtml)
      } yield AutoCompleteCtx(pxAutoComplete.value(), h.domCast[html.TextArea])

    override protected def getTextFromHeadToCaret =
      AutoComplete.getTextFromHeadToCaretTA

    private val onChange: ReactEventFromTextArea => Callback =
      e => {
        val txt = e.target.value
        for {
          p <- $.props
          _ <- p.state.modState(_.onTextChange(txt, p.group, p.tags))
        } yield ()
      }

    private val updateOnChange = ^.onChange ==> onChange

    private val container: TagMod => VdomNode = {
      val outer = <.div(^.cls := "ui form", *.derivativeTagsEditorContainer)
      val inner = <.div(^.cls := "field")
      m => (outer(inner(m)))
    }

    private val invalidityTitle: Invalidity => String = {
      case _: Invalidity.BadStatement   => "invalid definition"
      case _: Invalidity.Conflict       => "conflicting rules"
      case _: Invalidity.DeadSource
         | _: Invalidity.ExternalSource => "illegal tag"
      case _: Invalidity.SameSources    => "a tag combined with itself, always equals itself"
      case _: Invalidity.TagNotFound    => "invalid tag"
    }

    private def renderProblems[A](problems  : Set[A])(
                                  titleStyle: TagMod,
                                  bodyStyle : TagMod,
                                  suffix    : String,
                                  title     : A => String,
                                  body      : A => IterableOnce[String]): TagMod =
      TagMod.when(problems.nonEmpty) {
        val titleLI = <.li(titleStyle)
        val bodyLI  = <.li(bodyStyle)
        var content = Multimap.empty[String, Set, String]

        for (a <- problems)
          content = content.addvs(title(a), body(a).iterator.toSet)

        val lis =
          MutableArray(content.keys)
            .sort
            .iterator()
            .toTagMod { t =>
              val c = content(t)
              val children =
                TagMod.when(c.nonEmpty) {
                  val lis = MutableArray(c).sort.iterator().toTagMod(bodyLI(_))
                  <.ul(lis)
                }
              titleLI(suffix + t, children)
            }

        <.ul(lis)
      }

    def render(p: Props): VdomNode = {
      import Invalidity._
      import Warning._

      val s        = p.state.value
      val txt      = IR.toText(s.ir, p.tags)
      val vali     = s.validate(p.tags.tagGroupTagsFDV(p.group))
      val validity = Valid when vali.invalidity.isEmpty

      val editor =
        editorRef.component(TagMod(
          *.derivativeTagsEditor(validity),
          updateOnChange,
          ^.onBlur     --> autoCompleteOnBlur,
          ^.onClick    ==> autoCompleteOnClick,
          ^.onKeyDown  ==> autoCompleteOnKeyDown,
          ^.spellCheck  := false,
          ^.placeholder := "Define rules to combine tags by typing:\nrule1 + rule2 = newRule",
          ^.value       := txt,
        ))

      val showTag: TagId => String =
        p.tags.needTag(_).name

      def tagIsDead    (id: ApplicableTagId) = s"${showTag(id)} is deleted"
      def tagIsExternal(id: ApplicableTagId) = s"${showTag(id)} isn't a ${showTag(p.group).toLowerCase}"

      val warningTitle: Warning => String = {
        case DeadTarget    (id) => tagIsDead(id)
        case ExternalTarget(id) => tagIsExternal(id)
        case ExplicitRefl  (id) => s"${showTag(id)} + ${showTag(id)} = ${showTag(id)} is redundant"
      }

      val invalidityBody: Invalidity => IterableOnce[String] = {
        case BadStatement  (s)  => IR.toText(s, p.tags) :: Nil
        case Conflict      (ss) => ss.iterator.map(IR.toText(_, p.tags))
        case DeadSource    (id) => tagIsDead(id) :: Nil
        case ExternalSource(id) => tagIsExternal(id) :: Nil
        case SameSources   (id) => s"${showTag(id)} + ${showTag(id)}" :: Nil
        case TagNotFound   (t)  => t :: Nil
      }

      val warnings =
        renderProblems(
          problems   = vali.warnings)(
          titleStyle = *.derivativeTagsEditorWarningTitle,
          bodyStyle  = *.derivativeTagsEditorWarningBody,
          suffix     = "Warning: ",
          title      = warningTitle,
          body       = _ => Nil)

      val errors =
        renderProblems(
          problems   = vali.invalidity)(
          titleStyle = *.derivativeTagsEditorErrorTitle,
          bodyStyle  = *.derivativeTagsEditorErrorBody,
          suffix     = "Error: ",
          title      = invalidityTitle,
          body       = invalidityBody)

      container(TagMod(
        editor,
        warnings,
        errors))
    }
  }

  // ===================================================================================================================

  implicit val reusabilityState: Reusability[State] = Reusability.derive
  implicit val reusabilityProps: Reusability[Props] = Reusability.derive

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .configure(AutoComplete.install)
    .build
}
