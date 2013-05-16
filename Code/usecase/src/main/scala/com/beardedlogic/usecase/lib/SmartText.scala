package com.beardedlogic.usecase.lib

import scala.annotation.tailrec
import scala.collection.immutable.{Set, TreeSet}
import scala.util.parsing.combinator.RegexParsers
import net.liftweb.actor.LiftActor
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._

import JsExt._
import msg.MessageCentre
import msg.Messages._

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

  @inline def MakeInvalidRef(label: String) = label + "?"
  @inline def MakeInvalidRef(sb: StringBuilder, label: String) = {
    sb += RefBraceL
    sb ++= label
    sb += '?'
    sb += RefBraceR
  }

  val DeletedRef = MakeRef("DELETED")

  val FlowToArrowRegex = "-->|→".r
  val FlowToArrowBadReplacement = "->"
  val FlowToArrowGoodReplacement = "→"

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
        sb += in.first;
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

    val FlowToArrow: Parser[String] = FlowToArrowRegex

    val FlowToRefList: Parser[List[String]] = rep1sep(OptionallyBracedRef, "," ?)

    val TextAndFlowToTargets: Parser[(String, List[String])] = AnyTextThen(false, FlowToArrow ~> FlowToRefList)

    /**
     * Matches Text and the first step reference. If no refs, then matches the entire input as Text.
     */
    val TextAndPossibleRef: Parser[(String, Option[String])] = AnyTextThenOptional(true, BracedRef)
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
                 ) extends LiftActor {

  import SmartText._
  import MyLittleParser._

  protected val writeLock = new Object
  protected[lib] var refAndIdLookup = Map.empty[String, String]
  protected[lib] var refsInText = Map.empty[String, String]

  protected[lib] var _text = ""

  def text = _text

  def text_=(newValueRaw: String) {
    val newValue = newValueRaw.trim
    if (text != newValue) {
      _text = parseText(newValue)
    }
  }

  def init() {
    msgCentre.register(this)
    refAndIdLookup = refAndIdLookupProvider()
    _text = parseText(_text)
  }

  def renderTextarea = SHtml.ajaxTextarea(text, onTextChange _, "id" -> textareaId)

  /**
   * Callback when the user changes the text.
   */
  def onTextChange(newValue: String): JsCmd = writeLock.synchronized {
    refAndIdLookup = refAndIdLookupProvider() // StepChangeMsg only updates if we have refs
    text = newValue
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
        if (!refsInText.contains(label)) refsInText += (label -> refAndIdLookup(label))
        MakeRef(newText, label)
      } else
        MakeRef(newText, MakeInvalidRef(label))

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
    case StepChangeMsg if refsInText.nonEmpty =>

      // Ignore changes if already processed
      val newRefLookup = refAndIdLookupProvider()
      if (newRefLookup != refAndIdLookup) writeLock.synchronized {

        // Update step references
        var newRefsInText = Map.empty[String, String]
        var newText = text
        for ((oldLabel, id) <- refsInText) {

          // Lookup each existing reference
          newRefLookup.get(id).map { newLabel =>
            if (oldLabel != newLabel)
              newText = newText.replace(MakeRef(oldLabel), MakeRef(newLabel))
            if (!newRefsInText.contains(newLabel)) newRefsInText += (newLabel -> id)
          } orElse {
            newText = newText.replace(MakeRef(oldLabel), DeletedRef)
            None
          }
        }

        // Save and publish text changes
        if (newText != text) {
          _text = newText
          refsInText = newRefsInText
          msgCentre ! PushToClient(updateTextJs)
        }

        refAndIdLookup = newRefLookup
      }
  }

  private def updateTextJs(): JsCmd = JqId(textareaId) ~> JqSetValue(text, false)
}

// =====================================================================================================================

/**
 * Extended implementation for step text-fields.
 *
 * @param stepId The ID of the owning step.
 */
class SmartStepText(override val msgCentre: MessageCentre,
                    override val refAndIdLookupProvider: () => Map[String, String],
                    val stepId: String,
                    override val textareaId: String
                     ) extends SmartText(msgCentre, refAndIdLookupProvider, textareaId) {

  import SmartText._
  import MyLittleParser._

  private[lib] var flowToRefs = Set.empty[String]

  /**
   * Parses text submitted by user.
   */
  override protected def parseText(origText: String): String = {
    var (text, textSuffix) = parseTextForFlowTo(origText)
    text = parsePlainText(text)
    List(text, textSuffix).filterNot(_.isEmpty).mkString(" ")
  }

  /**
   * Scans input for an optional "--> 1.0.2" suffix indicating to which steps the current step can flow.
   *
   * If found (and valid), the suffix is extracted and normalised.
   */
  private def parseTextForFlowTo(input: String): (String, String) = {
    var (text, suffix) = (input, "")

    val prevFlowToRefs = flowToRefs
    flowToRefs = Set.empty

    val p = parseAll(TextAndFlowToTargets, input)
    if (p.successful) {
      val (actualText, labels) = p.get
      if (areAllLabelsValid(labels)) {
        flowToRefs = labels.map(refAndIdLookup(_)).toSet
        val sortedLabels = TreeSet(labels: _*).mkString(", ")
        text = actualText.trim
        suffix = s"$FlowToArrowGoodReplacement $sortedLabels"
      }
    }

    text = FlowToArrowRegex.replaceAllIn(text, FlowToArrowBadReplacement)

    if (flowToRefs != prevFlowToRefs) {
      msgCentre ! FlowToChangeMsg(flowToRefs, stepId)
    }

    (text, suffix)
  }

  override def messageHandler = thisMessageHandler orElse super.messageHandler
  private val thisMessageHandler: PartialFunction[Any, Unit] = {
    case FlowToChangeMsg =>
  }
}