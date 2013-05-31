package com.beardedlogic.usecase.lib

import scala.annotation.tailrec
import scala.collection.immutable.{Set, TreeSet}
import scala.util.parsing.combinator.RegexParsers
import net.liftweb.actor.LiftActor
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers.nextFuncName
import net.liftweb.common.Logger

import JsExt._
import msg.MessageCentre
import msg.Messages._
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

  val FlowToArrowRegex = "-{2,}>|[→➡⇨⇒⇾]".r
  val FlowToArrowBadReplacement = "->"
  val FlowToArrow = "➡"

  val FlowFromArrowRegex = "<-{2,}|[←⬅⇦⇐⇽]".r
  val FlowFromArrowBadReplacement = "<-"
  val FlowFromArrow = "⬅"

  @inline def MakeFlowText(arrow: String, labels: TreeSet[String]) =
    arrow + " " + labels.map(MakeRef(_)).mkString(" ")

  @inline def MakeFlowTextOrEmpty(arrow: String, labels: TreeSet[String]) =
    if (labels.isEmpty) "" else MakeFlowText(arrow, labels)

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

    val StepLabelComponent: Parser[String] = "[A-Za-z]+|\\d+".r // TODO remove caps?

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
                val refAndIdLookupProvider: () => Map[String, String],
                val textareaId: String = nextFuncName
                 ) extends LiftActor with Logger {

  import SmartText._
  import MyLittleParser._

  protected val writeLock = new Object
  protected[lib] var refAndIdLookup = Map.empty[String, String]
  protected[lib] var refsInText = Map.empty[String, String @@ LocalStepId]

  protected[lib] var _text = ""
  protected[lib] var _textWithNormalisedRefs = "".hasNormalisedRefs
  def textWithNormalisedRefs: String @@ NormalisedRefs = _textWithNormalisedRefs

  def text = _text
  private var _allowBroadcasting = false
  final def allowBroadcasting_? = _allowBroadcasting

  def init() {
    msgCentre.register(this)
    _allowBroadcasting = true
    refAndIdLookup = refAndIdLookupProvider()
    _text = parseText(_text)
  }

  def renderTextarea = SHtml.ajaxTextarea(text, onTextChange _, "id" -> textareaId)

  /**
   * Normalises and parses text from the user.
   */
  def setTextFromUser(newValueRaw: String) {
    val newValue = newValueRaw.trim
    if (text != newValue) {
      _text = parseText(newValue)
    }
  }

  /**
   * Restores internal state to a previous state. Usually called when loading from DB.
   *
   * @param newValueWithNRefs A text value with all step references normalised with DB data IDs instead of
   *                          human-readable labels.
   * @param savedSteps A map of step data-to-node ids.
   */
  def setTextFromLoad(newValueWithNRefs: String @@ NormalisedRefs, savedSteps: BiMap[Long_StepDataId, String @@ LocalStepId]) {
    refAndIdLookup = refAndIdLookupProvider()
    _textWithNormalisedRefs = newValueWithNRefs

    // Realised normalised refs
    val newValue = NormalisedRefRegex.replaceAllIn(newValueWithNRefs, { m =>
      val dataIdText = m.group(1)
      val dataId = dataIdText.toLong.tag[StepDataId]
      savedSteps.ab.get(dataId).flatMap(nodeId => refAndIdLookup.get(nodeId)).map(MakeRef(_)).getOrElse{
        warn(s"Unable to realise normalised step reference. ❚ Text: $newValueWithNRefs ❚ DataId: $dataId ❚ SavedSteps: $savedSteps ❚ RefAndIdLookup: $refAndIdLookup")
        MakeInvalidNormalisedRef(dataIdText)
      }
    })

    // Parse text as normal
    disableBroadcasting {
      _text = parseText(newValue)
    }
  }

  def recalcTextWithNormalisedRefs(savedSteps: Map[String @@ LocalStepId, Long_StepDataId]) {
    _textWithNormalisedRefs = normaliseRefs(text, savedSteps, refsInText)
  }

  protected def normaliseRefs(
    text: String,
    savedSteps: Map[String @@ LocalStepId, Long_StepDataId],
    refs: Map[String, String @@ LocalStepId]): String @@ NormalisedRefs = {

    var r = text
    for {
      (label, localId) <- refs
      dataId <- savedSteps.get(localId)
    } r = r.replace(MakeRef(label), MakeNormalisedRef(dataId))
    r.hasNormalisedRefs
  }

  def disableBroadcasting[T](block: => T) = {
    val i = _allowBroadcasting
    _allowBroadcasting = false
    try block
    finally _allowBroadcasting = i
  }

  /**
   * Sets the full text value and pushes the new text to the client.
   *
   * Unlike `setTextFromUser()` this doesn't perform any parsing. The only variable this changes is `_text`.
   */
  @inline protected def internalSetTextAndPush(newText: String) {
    _text = newText
    if (allowBroadcasting_?) msgCentre ! PushToClient(updateTextJs)
  }

  /**
   * Callback when the user changes the text.
   */
  def onTextChange(newValue: String): JsCmd = writeLock.synchronized {
    refAndIdLookup = refAndIdLookupProvider() // Shouldn't be needed but no harm and provides extra safety
    setTextFromUser(newValue)
    if (text != newValue)
      updateTextJs
    else
      JsCmds.Noop
  }

  /**
   * Parses text submitted by user.
   */
  protected def parseText(origText: String): String = {
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
      val label = r.get._2.get
      if (refAndIdLookup.contains(label)) {
        if (!refsInText.contains(label)) refsInText += (label -> refAndIdLookup(label).asLocalStepId)
        MakeRef(newText, label)
      } else
        MakeInvalidRef(newText, label)

      // Continue parsing
      r = parse(TextAndPossibleRef, r.next)
    }
    newText ++= r.get._1

    newText.toString
  }

  @inline protected final def areAllLabelsValid(labels: Seq[String]): Boolean = {
    labels.find(!refAndIdLookup.contains(_)).isEmpty
  }

  override def messageHandler = {

    case StepChangeMsg =>

      // Ignore changes if already processed
      val newRefLookup = refAndIdLookupProvider()
      if (newRefLookup != refAndIdLookup) {

        // Save new steps
        refAndIdLookup = newRefLookup

        // Update references and push changes
        if (haveAnyRefs) writeLock.synchronized {
          val newText = updateStepReferences()
          if (newText != text) internalSetTextAndPush(newText)
        }
      }
  }

  /** Checks if this field has any step references */
  protected def haveAnyRefs = refsInText.nonEmpty

  /** Updates `refsInText` and creates a copy of `text` in which all references are up-to-date. */
  protected def updateStepReferences(): String = updateStepReferences(text)

  /**
   * Updates `refsInText` and creates a copy of given text in which all references are up-to-date.
   */
  protected def updateStepReferences(text: String): String = {
    var newRefsInText = Map.empty[String, String @@ LocalStepId]
    var newText = text
    for ((oldLabel, id) <- refsInText) {

      // Lookup each existing reference
      refAndIdLookup.get(id).map { newLabel =>
        if (oldLabel != newLabel)
          newText = newText.replace(MakeRef(oldLabel), MakeRef(newLabel))
        if (!newRefsInText.contains(newLabel))
          newRefsInText += (newLabel -> id)
      } orElse {
        newText = newText.replace(MakeRef(oldLabel), DeletedRef)
        None
      }
    }

    refsInText = newRefsInText
    newText
  }

  protected def updateTextJs(): JsCmd = JqId(textareaId) ~> JqSetValue(text, false)
}

// =====================================================================================================================

/**
 * Extended implementation for step text-fields.
 *
 * @param stepId The ID of the owning step.
 */
class SmartStepText(override val msgCentre: MessageCentre,
                    override val refAndIdLookupProvider: () => Map[String, String],
                    val stepId: String @@ LocalStepId,
                    override val textareaId: String
                     ) extends SmartText(msgCentre, refAndIdLookupProvider, textareaId) {

  import SmartText._
  import MyLittleParser._

  /**
   * Common interface for providing step-flow functionality.
   * Stores data for a single flow.
   * Allows for flow-agnostic logic.
   */
  sealed trait Flow {
    var refs = Map.empty[String @@ LocalStepId, String]
    var text = ""
    def arrow : String
    def arrowReplacement : String
    def get(pr : ParseResult[FlowParseResult]): Option[List[String]]
    def broadcast():Unit
    final def clear() {refs = Map.empty[String @@ LocalStepId, String]; text = ""}
    final def broadcastIfChanges[T](block: => T): T = {
      val prevRefs = refs
      val r = block
      if (prevRefs != refs) broadcast
      r
    }
    final def clearAndBroadcast() {
      if (refs.nonEmpty) {
        clear()
        broadcast
      }
    }
    final def sortedLabels: TreeSet[String] = {
      var s = TreeSet.empty[String]
      for (lbl <- refs.values) s += lbl
      s
    }
    final def rebuildText() { text = MakeFlowTextOrEmpty(arrow, sortedLabels) }
  }

  /** Indicates which steps flow into this step. */
  final class FlowFrom extends Flow {
    override def arrow = FlowFromArrow
    override def arrowReplacement = FlowFromArrowBadReplacement
    override def get(pr : ParseResult[FlowParseResult]) = pr.get.from
    override def broadcast() { if (allowBroadcasting_?) msgCentre ! FlowFromChangeMsg(refs.keySet, stepId) }
  }

  /** Indicates into which steps this step flows. */
  final class FlowTo extends Flow {
    override def arrow = FlowToArrow
    override def arrowReplacement = FlowToArrowBadReplacement
    override def get(pr : ParseResult[FlowParseResult]) = pr.get.to
    override def broadcast() { if (allowBroadcasting_?) msgCentre ! FlowToChangeMsg(stepId, refs.keySet) }
  }

  private[lib] var textWithoutFlow = ""
  private[lib] val flowFrom = new FlowFrom
  private[lib] val flowTo = new FlowTo

  /**
   * Parses text submitted by user.
   */
  override protected def parseText(origText: String): String = {
    val plainText = parseTextForFlow(origText)
    textWithoutFlow = parsePlainText(plainText)
    buildFullText
  }

  /** Combines text & flow clauses to generate `text`. */
  def buildFullText = List(textWithoutFlow, flowFrom.text, flowTo.text).filterNot(_.isEmpty).mkString(" ")

  /**
   * Scans input for optional flow clauses such as `"--> 1.0.2"`, `"<-- 1.4, 1.5"`.
   *
   * If found (and valid), they are extracted and normalised.
   */
  private def parseTextForFlow(input: String) = {
    var text = input

    // Attempt to parse flow clauses
    val pr = parseAll(TextAndFlow, input)
    if (pr.successful) {

      // Clauses exist. Validate.
      val (actualText, flowResult) = pr.get
      val fn1 = processFlowParseResult(flowResult.from, flowFrom)
      val fn2 = if (fn1.isEmpty) None else processFlowParseResult(flowResult.to, flowTo)
      if (fn2.isDefined) {

        // No errors in From or To clauses. Apply.
        text = actualText.trim
        (fn1.get)()
        (fn2.get)()
      }

    } else {
      // Wipe previous flow refs
      flowFrom.clearAndBroadcast
      flowTo.clearAndBroadcast
    }

    text = FlowFromArrowRegex.replaceAllIn(text, FlowFromArrowBadReplacement)
    text = FlowToArrowRegex.replaceAllIn(text, FlowToArrowBadReplacement)
    text
  }

  private def processFlowParseResult(labelsOp: Option[List[String]], f: Flow): Option[Function0[Unit]] =
    labelsOp match {
      case None =>
        Some(f.clearAndBroadcast _)

      case Some(labels) if (areAllLabelsValid(labels)) =>
        Some(() => f.broadcastIfChanges {
          f.refs = labels.map(l => (refAndIdLookup(l).asLocalStepId, l)).toMap
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
      refAndIdLookup.get(kp._1).map(_ != kp._2).getOrElse(true)
    }
    if (changeFound) {
      var newLabels = TreeSet.empty[String]
      var newRefs = Map.empty[String @@ LocalStepId, String]
      for ((id,_) <- f.refs) {
        if (refAndIdLookup.contains(id)) {
          val l = refAndIdLookup(id)
          newRefs += (id -> l)
          newLabels += l
        } else {
          // step deleted, just omit
        }
      }
      f.refs = newRefs
      f.text = MakeFlowTextOrEmpty(f.arrow, newLabels)
    }
  }

  override def messageHandler = thisMessageHandler orElse super.messageHandler
  private val thisMessageHandler: PartialFunction[Any, Unit] = {

    // Add or Remove flow references
    case FlowFromChangeMsg(_, id) if id == stepId => // Ignore self-ref
    case FlowToChangeMsg(id, _) if id == stepId => // Ignore self-ref
    case FlowToChangeMsg(id, toIds) if (toIds.contains(stepId) && !flowFrom.refs.contains(id)) => addRef(flowFrom, id)
    case FlowToChangeMsg(id, toIds) if (!toIds.contains(stepId) && flowFrom.refs.contains(id)) => removeRef(flowFrom, id)
    case FlowFromChangeMsg(fromIds, id) if (fromIds.contains(stepId) && !flowTo.refs.contains(id)) => addRef(flowTo, id)
    case FlowFromChangeMsg(fromIds, id) if (!fromIds.contains(stepId) && flowTo.refs.contains(id)) => removeRef(flowTo, id)
  }

  private def addRef(f: Flow, id: String @@ LocalStepId) {
    f.refs += (id -> refAndIdLookup(id))
    f.rebuildText
    internalSetTextAndPush(buildFullText)
  }

  private def removeRef(f: Flow, id: String @@ LocalStepId) {
    f.refs -= id
    f.rebuildText
    internalSetTextAndPush(buildFullText)
  }
}