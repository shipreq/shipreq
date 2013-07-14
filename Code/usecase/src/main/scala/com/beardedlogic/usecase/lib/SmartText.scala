package com.beardedlogic.usecase.lib

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.util.parsing.combinator.RegexParsers
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers.nextFuncName
import net.liftweb.common.Logger

import com.beardedlogic.usecase.util._
import JsExt._
import Messages._
import TypeTags._


// =====================================================================================================================

object SmartText {

  val RefBraceL = '['
  val RefBraceR = ']'

  @inline def MakeRef(label: String) = RefBraceL + label + RefBraceR
  @inline def MakeRef(sb: StringBuilder, label: String) {
    sb += RefBraceL
    sb ++= label
    sb += RefBraceR
  }

  @inline def MakeInvalidLabel(label: String) = label + "?"
  @inline def MakeInvalidRef(sb: StringBuilder, label: String) = {
    sb += RefBraceL
    sb ++= label
    sb += '?'
    sb += RefBraceR
  }

  val NormalisedRefRegex = "\\[D\\.(\\d+?)\\]".r
  def MakeNormalisedRef(dataId: Long_StepDataId) = MakeRef("D." + dataId)
  def MakeInvalidNormalisedRef(dataId: String) = MakeRef("D." + dataId)

  val DeletedRef = MakeRef("DELETED")

  // TODO Merge flow arrow stuff
  val FlowToArrow = "➡"
  val FlowToUnicodeArrows: List[Char] = (FlowToArrow + "→⇨⇒⇾").toList
  val FlowToArrowRegex = s"-{2,}>|[${FlowToUnicodeArrows.mkString}]".r
  val FlowToArrowBadRegex = s"-*(?:->|[${FlowToUnicodeArrows.mkString}])".r
  val FlowToArrowBadReplacement = "->"

  val FlowFromArrow = "⬅"
  val FlowFromUnicodeArrows: List[Char] = (FlowFromArrow + "←⇦⇐⇽").toList
  val FlowFromArrowRegex = s"<-{2,}|[${FlowFromUnicodeArrows.mkString}]".r
  val FlowFromArrowBadRegex = s"(?:<-|[${FlowFromUnicodeArrows.mkString}])-*".r
  val FlowFromArrowBadReplacement = "<-"

  @inline def MakeFlowText(arrow: String, labels: TreeSet[LabelStr]) =
    arrow + " " + labels.map(MakeRef(_)).mkString(" ")

  @inline def MakeFlowTextOrEmpty(arrow: String, labels: TreeSet[LabelStr]) =
    if (labels.isEmpty) "" else MakeFlowText(arrow, labels)

  def normaliseRefs(
    text: String,
    savedSteps: Map[LocalIdStr, Long_StepDataId],
    refs: Map[LocalIdStr, LabelStr]): String @@ NormalisedRefs = {

    var r = text
    for {
      (localId, label) <- refs
      dataId <- savedSteps.get(localId)
    } r = r.replace(MakeRef(label), MakeNormalisedRef(dataId))
    r.hasNormalisedRefs
  }

  /**
   * My Little <strike>Pony</strike> Parser here expresses the syntax that enables various special features to sprout
   * from plain UC text.
   *
   * @since 15/05/2013
   */
  object MyLittleParser extends RegexParsers {

    // Gobbles whitespace. In plain-text we need whitespace preserved.
    @inline private def gobbleWhitespace(sb: StringBuilder, _in: Input) = {
      var in = _in
      while (!in.atEnd && Character.isWhitespace(in.first)) {
        sb += in.first
        in = in.rest
      }
      in
    }

    /**
     * Non-greedily matches 0-n characters, followed by another given matcher.
     *
     * (In-built parsers are all greedy; even ".*?".r won't work.)
     *
     * @param keepText If true, whitespace at the end of the text will be preserved.
     * @param nextParser The parser that matches after this. It must succeed for this to stop.
     * @tparam T The type of the next parser.
     * @return A tuple of the characters collected here (can be an empty string), and the result of nextParser.
     */
    def AnyTextThen[T](keepText: Boolean, nextParser: Parser[T]) = Parser[(String, T)] { in =>
      val sb = new StringBuilder
      @tailrec def parse(_in: Input): ParseResult[(String, T)] = {
        val in = if (keepText) gobbleWhitespace(sb, _in) else _in
        nextParser(in) match {
          case Success(a, rest) => Success((sb.toString, a), rest)
          case e@Error(_, _)    => e // still have to propagate error
          case _ if (in.atEnd)  => Failure("end of input", in)
          case _                => sb += in.first; parse(in.rest)
        }
      }
      parse(in)
    }

    /**
     * Non-greedily matches 0-n characters, optionally followed by another given matcher.
     *
     * In-built parsers are all greedy; even ".*?".r won't work.
     *
     * @param keepText If true, whitespace at the end of the text will be preserved.
     * @param nextParser The parser that may match after this. If it succeeds, this stops; else this will collect the
     *                   entire string.
     * @tparam T The type of the next parser.
     * @return A tuple of the characters collected here (can be an empty string), and the result of nextParser.
     */
    def AnyTextThenOptional[T](keepText: Boolean, nextParser: Parser[T]) = Parser[(String, Option[T])] { in =>
      val sb = new StringBuilder
      @tailrec def parse(_in: Input): ParseResult[(String, Option[T])] = {
        val in = if (keepText) gobbleWhitespace(sb, _in) else _in
        nextParser(in) match {
          case Success(a, rest) => Success((sb.toString, Some(a)), rest)
          case e@Error(_, _)    => e // still have to propagate error
          case _ if (in.atEnd)  => Success((sb.toString, None), in)
          case _                => sb += in.first; parse(in.rest)
        }
      }
      parse(in)
    }

    val StepLabelComponent: Parser[String] = "[A-Za-z]+|\\d+".r

    val StepLabel: Parser[String] = StepLabelComponent ~ rep1("." ~> StepLabelComponent) ^^ {
      case h ~ t => (h :: t).mkString(".")
    }

    val BracedRef: Parser[String] = "[" ~> StepLabel <~ "]"

    val OptionallyBracedRef: Parser[String] = BracedRef | StepLabel

    val FlowRefList: Parser[List[String]] = rep1sep(OptionallyBracedRef, "," ?)

    val FlowFromClause: Parser[List[String]] = FlowFromArrowRegex ~> FlowRefList

    val FlowToClause: Parser[List[String]] = FlowToArrowRegex ~> FlowRefList

    val TextAndFlow: Parser[(String, FlowParseResult)] =
      AnyTextThen(false,
                   FlowFromClause ~ opt(FlowToClause) ^^ { case from ~ to => FlowParseResult(Some(from), to) } |
                     FlowToClause ~ opt(FlowFromClause) ^^ { case to ~ from => FlowParseResult(from, Some(to)) }
                 )

    /**
     * Matches Text and the first step reference. If no refs, then matches the entire input as Text.
     */
    val TextAndPossibleRef: Parser[(String, Option[String])] = AnyTextThenOptional(true, BracedRef)

    case class FlowParseResult(from: Option[List[String]], to: Option[List[String]])
  }

}

// =====================================================================================================================

/**
 * Encapsulates a String to provide the following functionality:
 * <ul>
 * <li>References to steps in the text are validated, invalid references are transformed to make the invalidity
 * obvious.</li>
 * <li>References to steps are updated when their labels change, and the updated text is pushed back to the client.</li>
 * </ul>
 *
 * Make sure you call <code>init()</code> before use.
 *
 * @since 12/05/2013
 */
class SmartText(val msgCentre: MessageCentre,
                masterRefAndIdLookup: CachedFunctionLike[BiMap[LocalIdStr, LabelStr]],
                val textareaId: String = nextFuncName
                 ) extends MessageListener with Logger {

  import SmartText._
  import MyLittleParser._

  // TODO Revise SmartText.writeLock strategy
  protected val writeLock = new Object
  protected[lib] var _text = ""
  protected[lib] var refsInText = Map.empty[LocalIdStr, LabelStr]
  protected[lib] val refAndIdLookup = masterRefAndIdLookup.dependentCopyLazy

  def text = _text

  def init() {
    msgCentre.register(this)
    refAndIdLookup.invalidate
    _text = parseText(_text)(NoReactionOrNewMessages)
  }

  def renderTextarea = SHtml.ajaxTextarea(text, onTextChange _, "id" -> textareaId)

  /**
   * Restores internal state to a previous state. Usually called when loading from DB.
   *
   * @param newValueWithNRefs A text value with all step references normalised with DB data IDs instead of
   *                          human-readable labels.
   * @param savedSteps A map of step data-to-node ids.
   */
  def setTextFromLoad(newValueWithNRefs: String @@ NormalisedRefs, savedSteps: BiMap[Long_StepDataId, LocalIdStr]) {
    refAndIdLookup.invalidate

    // Realise normalised refs
    val newValue = NormalisedRefRegex.replaceAllIn(newValueWithNRefs, { m =>
      val dataIdText = m.group(1)
      val dataId = dataIdText.toLong.tag[StepDataId]
      savedSteps.ab.get(dataId).flatMap(nodeId => refAndIdLookup.get.ab.get(nodeId)).map(MakeRef(_)).getOrElse{
        warn(s"Unable to realise normalised step reference. ❚ Text: $newValueWithNRefs ❚ DataId: $dataId ❚ SavedSteps: $savedSteps ❚ RefAndIdLookup: $refAndIdLookup")
        MakeInvalidNormalisedRef(dataIdText)
      }
    })

    // Parse text as normal
    _text = parseText(newValue)(NoReactionOrNewMessages)
  }

  @inline final def textWithNormalisedRefs(ucCtx: UseCaseCtx): String @@ NormalisedRefs =
    textWithNormalisedRefs(ucCtx.savedSteps.get.ba)

  def textWithNormalisedRefs(savedSteps: Map[LocalIdStr, Long_StepDataId]): String @@ NormalisedRefs =
    normaliseRefs(text, savedSteps, refsInText)

  /**
   * Callback when the user changes the text.
   */
  def onTextChange(newValue: String): JsCmd = writeLock.synchronized {
    refAndIdLookup.invalidate // Shouldn't be needed but no harm and provides extra safety
    JavaScriptReaction { setTextFromUser(newValue)(_) }
  }

  /**
   * Normalises and parses text from the user.
   */
  def setTextFromUser(newValueRaw: String)(implicit reactor: Reactor) {
    val newValue = newValueRaw.trim
    if (text != newValue) {
      _text = parseText(newValue)
      if (text != newValue) reactToTextUpdate
    }
  }

  /**
   * Parses text submitted by user.
   */
  protected def parseText(origText: String)(implicit reactor: Reactor): String = {
    parsePlainText(origText)
  }

  /**
   * Parses a plan text.
   *
   * Records a map-to-ids of valid references in curRefLookup.
   * Removes whitespace from references.
   * Appends a ? to invalid references.
   */
  protected def parsePlainText(text: String): String = {
    val newText = new StringBuilder
    refsInText = Map.empty

    // Parse input
    var r = parse(TextAndPossibleRef, text)
    while (r.get._2.isDefined) {
      newText ++= r.get._1

      // Check label validity
      val label = r.get._2.get.asLabel
      val id = refAndIdLookup.get.ba.get(label)
      if (id.isDefined) {
        if (!refsInText.contains(id.get)) refsInText += (id.get -> label)
        MakeRef(newText, label)
      } else
        MakeInvalidRef(newText, label)

      // Continue parsing
      r = parse(TextAndPossibleRef, r.next)
    }
    newText ++= r.get._1

    newText.toString
  }

  override def messageHandler(reactor: Reactor): PartialFunction[Message, Unit] = {

    // Update step references when they change
    case StepChangeMsg =>
      refAndIdLookup.ifStale {
        if (haveAnyRefs) writeLock.synchronized {
          val newText = updateStepReferences()
          if (newText != text) internalSetTextAndReact(newText)(reactor)
        }
      }
  }

  /**
   * Sets the full text value and pushes the new text to the client.
   *
   * Unlike `setTextFromUser()` this doesn't perform any parsing. The only variable this changes is `_text`.
   */
  @inline final protected def internalSetTextAndReact(newText: String)(implicit reactor: Reactor) {
    _text = newText
    reactToTextUpdate
  }

  @inline protected final def areAllLabelsValid(labels: Seq[LabelStr]): Boolean =
    labels.find(!refAndIdLookup.get.ba.contains(_)).isEmpty

  /** Checks if this field has any step references */
  protected def haveAnyRefs = refsInText.nonEmpty

  /** Updates `refsInText` and creates a copy of `text` in which all references are up-to-date. */
  protected def updateStepReferences(): String = updateStepReferences(text)

  /**
   * Updates `refsInText` and creates a copy of given text in which all references are up-to-date.
   */
  protected def updateStepReferences(text: String): String = {
    var newRefsInText = Map.empty[LocalIdStr, LabelStr]
    var newText = text
    for ((id, oldLabel) <- refsInText) {

      // Lookup each existing reference
      refAndIdLookup.get.ab.get(id).map { newLabel =>
        if (oldLabel != newLabel)
          newText = newText.replace(MakeRef(oldLabel), MakeRef(newLabel))
        if (!newRefsInText.contains(id))
          newRefsInText += (id -> newLabel)
      } orElse {
        newText = newText.replace(MakeRef(oldLabel), DeletedRef)
        None
      }
    }

    refsInText = newRefsInText
    newText
  }

  protected def updateTextJs(): JsCmd = JqId(textareaId) ~> JqSetValue(text, false)

  @inline final protected def reactToTextUpdate(implicit reactor: Reactor) {
    reactor(JavaScript)(updateTextJs)
  }
}

// =====================================================================================================================

/**
 * Extended implementation for step text-fields.
 *
 * @param stepId The ID of the owning step.
 */
class SmartStepText(override val msgCentre: MessageCentre,
                    masterRefAndIdLookup: CachedFunctionLike[BiMap[LocalIdStr, LabelStr]],
                    val stepId: LocalIdStr,
                    override val textareaId: String
                     ) extends SmartText(msgCentre, masterRefAndIdLookup, textareaId) {

  import SmartText._
  import MyLittleParser._

  /**
   * Common interface for providing step-flow functionality.
   * Stores data for a single flow.
   * Allows for flow-agnostic logic.
   */
  sealed trait Flow {
    var refs = Map.empty[LocalIdStr, LabelStr]
    var text = ""
    def arrow : String
    // def arrowReplacement : String
    def get(pr : ParseResult[FlowParseResult]): Option[List[String]]
    def flowChangeMsg: Message
    final def broadcast(implicit reactor: Reactor) { msgCentre ! flowChangeMsg }
    final def clear() {refs = Map.empty[LocalIdStr, LabelStr]; text = ""}
    final def broadcastIfChanges[T](block: => T)(implicit reactor: Reactor): T = {
      val prevRefs = refs
      val r = block
      if (prevRefs != refs) broadcast
      r
    }
    final def clearAndBroadcast(implicit reactor: Reactor) {
      if (refs.nonEmpty) {
        clear()
        broadcast
      }
    }
    final def sortedLabels: TreeSet[LabelStr] = {
      var s = TreeSet.empty[LabelStr]
      for (lbl <- refs.values) s += lbl
      s
    }
    final def rebuildText() { text = MakeFlowTextOrEmpty(arrow, sortedLabels) }
  }

  /** Indicates which steps flow into this step. */
  final class FlowFrom extends Flow {
    override def arrow = FlowFromArrow
    // override def arrowReplacement = FlowFromArrowBadReplacement
    override def get(pr : ParseResult[FlowParseResult]) = pr.get.from
    override def flowChangeMsg = FlowFromChangeMsg(refs.keySet, stepId)
  }

  /** Indicates into which steps this step flows. */
  final class FlowTo extends Flow {
    override def arrow = FlowToArrow
    // override def arrowReplacement = FlowToArrowBadReplacement
    override def get(pr : ParseResult[FlowParseResult]) = pr.get.to
    override def flowChangeMsg = FlowToChangeMsg(stepId, refs.keySet)
  }

  private[lib] var textWithoutFlow = ""
  private[lib] val flowFrom = new FlowFrom
  private[lib] val flowTo = new FlowTo

  /**
   * Parses text submitted by user.
   */
  override protected def parseText(origText: String)(implicit reactor: Reactor): String = {
    val plainText = parseTextForFlow(origText)
    textWithoutFlow = parsePlainText(plainText)
    buildFullText
  }

  /** Combines text & flow clauses to generate `text`. */
  def buildFullText() = List(textWithoutFlow, flowFrom.text, flowTo.text).filterNot(_.isEmpty).mkString(" ")

  /**
   * Scans input for optional flow clauses such as `"--> 1.0.2"`, `"<-- 1.4, 1.5"`.
   *
   * If found (and valid), they are extracted and normalised.
   */
  private def parseTextForFlow(input: String)(implicit reactor: Reactor) = {
    var text = input

    // Attempt to parse flow clauses
    val pr = parseAll(TextAndFlow, input)
    if (pr.successful) {

      // Clauses exist. Validate.
      val (actualText, flowResult) = pr.get
      val fn1 = processFlowParseResult(flowResult.from.asLabels, flowFrom)
      val fn2 = if (fn1.isEmpty) None else processFlowParseResult(flowResult.to.asLabels, flowTo)
      if (fn2.isDefined) {

        // No errors in From or To clauses. Apply.
        text = actualText.trim
        (fn1.get)(reactor)
        (fn2.get)(reactor)
      }

    } else {
      // Wipe previous flow refs
      flowFrom.clearAndBroadcast
      flowTo.clearAndBroadcast
    }

    text = FlowFromArrowBadRegex.replaceAllIn(text, FlowFromArrowBadReplacement)
    text = FlowToArrowBadRegex.replaceAllIn(text, FlowToArrowBadReplacement)
    text
  }

  private def processFlowParseResult(labelsOp: Option[List[LabelStr]], f: Flow): Option[Reactor => Unit] =
    labelsOp match {
      case None =>
        Some(implicit reactor => f.clearAndBroadcast)

      case Some(labels) if (areAllLabelsValid(labels)) =>
        Some(implicit reactor => f.broadcastIfChanges {
          f.refs = labels.map(l => (refAndIdLookup.get.ba(l), l)).toMap
          f.rebuildText
        })

      case _ => None // Labels are invalid
    }

  override protected def haveAnyRefs = super.haveAnyRefs || flowFrom.refs.nonEmpty || flowTo.refs.nonEmpty

  override protected def updateStepReferences(): String = {
    updateFlowReferences(flowFrom)
    updateFlowReferences(flowTo)
    textWithoutFlow = updateStepReferences(textWithoutFlow)
    buildFullText
  }

  /**
   * Removes invalid references and creates a new flow clause (text).
   */
  private def updateFlowReferences(f: Flow) {
    val changeFound = f.refs.nonEmpty && f.refs.exists { kp =>
      refAndIdLookup.get.ab.get(kp._1).map(_ != kp._2).getOrElse(true)
    }
    if (changeFound) {
      var newLabels = TreeSet.empty[LabelStr]
      var newRefs = Map.empty[LocalIdStr, LabelStr]
      for ((id,_) <- f.refs) {
        refAndIdLookup.get.ab.get(id).map { l =>
          newRefs += (id -> l)
          newLabels += l
        }
        // orElse: step deleted, just omit
      }
      f.refs = newRefs
      f.text = MakeFlowTextOrEmpty(f.arrow, newLabels)
    }
  }

  override def messageHandler(reactor: Reactor) = subMessageHandler(reactor) orElse super.messageHandler(reactor)
  private def subMessageHandler(implicit reactor: Reactor): PartialFunction[Message, Unit] = {

    // Add or Remove flow references
    case FlowFromChangeMsg(_, id) if id == stepId => // Ignore self-ref
    case FlowToChangeMsg(id, _) if id == stepId   => // Ignore self-ref
    case FlowToChangeMsg(id, toIds)               => processFlowChangeMsg(toIds, id, flowFrom)
    case FlowFromChangeMsg(fromIds, id)           => processFlowChangeMsg(fromIds, id, flowTo)
  }

  private def processFlowChangeMsg(manyIds: Set[LocalIdStr], oneId: LocalIdStr, f: Flow)(implicit reactor: Reactor) {
    // Confusing but performant way of writing the following:
    //   case FlowToChangeMsg(id, toIds) if (toIds.contains(stepId) && !flowFrom.refs.contains(id)) => addRef(flowFrom, id)
    //   case FlowToChangeMsg(id, toIds) if (!toIds.contains(stepId) && flowFrom.refs.contains(id)) => removeRef(flowFrom, id)
    //   case FlowFromChangeMsg(fromIds, id) if (fromIds.contains(stepId) && !flowTo.refs.contains(id)) => addRef(flowTo, id)
    //   case FlowFromChangeMsg(fromIds, id) if (!fromIds.contains(stepId) && flowTo.refs.contains(id)) => removeRef(flowTo, id)
    val a = manyIds.contains(stepId)
    val b = f.refs.contains(oneId)
    if (a != b) {
      if (a) addRef(f, oneId) else removeRef(f, oneId)
      f.rebuildText
      internalSetTextAndReact(buildFullText)
    }
  }

  private def addRef(f: Flow, id: LocalIdStr)(implicit reactor: Reactor) {
    f.refs += (id -> refAndIdLookup.get.ab(id))
  }

  private def removeRef(f: Flow, id: LocalIdStr)(implicit reactor: Reactor) {
    f.refs -= id
  }

  override def textWithNormalisedRefs(savedSteps: Map[LocalIdStr, Long_StepDataId]): String @@ NormalisedRefs = {
    var txt = super.textWithNormalisedRefs(savedSteps)
    txt = normaliseRefs(txt, savedSteps, flowFrom.refs)
    txt = normaliseRefs(txt, savedSteps, flowTo.refs)
    txt
  }
}