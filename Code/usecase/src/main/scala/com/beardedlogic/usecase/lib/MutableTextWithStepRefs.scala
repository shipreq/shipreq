package com.beardedlogic.usecase.lib

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.util.parsing.combinator.RegexParsers
import net.liftweb.actor.LiftActor
import net.liftweb.common.Logger
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._

import JsExt._
import field.CourseFields.StepChangeMsg
import msg.{MessageCentre, PushToClient}

object MutableTextWithStepRefs {

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

  val ArrowRegex = "-->|→".r
  val ArrowBad = "->"
  val ArrowGood = "→"

  /**
   * My Little Pony here expresses the syntax that enables various special features to sprout from plain UC text.
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

    val linkNextArrow: Parser[String] = ArrowRegex

    val stepLabelComponent: Parser[String] = "[A-Za-z]+|\\d+".r // TODO remove caps?

    val stepLabel: Parser[String] = stepLabelComponent ~ rep1("." ~> stepLabelComponent) ^^ {
      case h ~ t => (h :: t).mkString(".")
    }

    // val stepLabel: Parser[String] = Parser { in =>
    //    stepLabelUnchecked(in) match {
    //      case s @ Success(lbl, _) => if (validLabels.contains(lbl)) s else Failure("Invalid label",in)
    //      case x => x
    //    }
    //  }

    val bracedRef: Parser[String] = "[" ~> stepLabel <~ "]"

    val optionallyBracedRef: Parser[String] = bracedRef | stepLabel

    val linkNextRefList: Parser[List[String]] = rep1sep(optionallyBracedRef, "," ?)

    val textWithLinkNext: Parser[(String, List[String])] = AnyTextThen(false, linkNextArrow ~> linkNextRefList)

    /**
     * Matches Text and the first step reference. If no refs, then matches the entire input as Text.
     */
    val textAndPossibleRef: Parser[(String, Option[String])] = AnyTextThenOptional(true, bracedRef)
  }

}

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
class MutableTextWithStepRefs(val msgCentre: MessageCentre,
                              val refLookupProvider: () => Map[String, String],
                              val id: String = nextFuncName
                               ) extends LiftActor {

  import MutableTextWithStepRefs._
  import MyLittleParser._

  private[lib] var curRefLookup = Map.empty[String, String]
  private[lib] var refsInText = Map.empty[String, String]
  private[lib] var refsInLinkNext = Map.empty[String, String]

  private[lib] var _text = ""

  def text = _text

  def text_=(newValueRaw: String) {
    val newValue = newValueRaw.trim
    if (text != newValue) {
      _text = parseText(newValue)
    }
  }

  def init() {
    msgCentre.register(this)
    _text = parseText(_text)
  }

  def renderTextarea = SHtml.ajaxTextarea(text, onTextChange _, "id" -> id)

  /**
   * Callback when the user changes the text.
   */
  def onTextChange(newValue: String): JsCmd = {
    text = newValue
    if (text != newValue)
      updateTextJs
    else
      JsCmds.Noop
  }

  /**
   * Scans text for step references.
   *
   * Creates a map-to-ids of valid references.
   * Removes whitespace from references.
   * Appends a ? to invalid references.
   */
  private def parseText(origText: String): String = {
    var (text, textSuffix) = parseTextLinkNext(origText)
    text = parsePlainText(text)
    List(text, textSuffix).filterNot(_.isEmpty).mkString(" ")
  }

  /**
   * Parses a plan text.
   * Step refs are normalised in text, and recorded in curRefLookup.
   */
  private def parsePlainText(text: String): String = {
    val refLookup = refLookupProvider()
    val newText = new StringBuilder
    refsInText = Map.empty

    // Parse input
    var r = parse(textAndPossibleRef, text)
    while (r.get._2.isDefined) {
      newText ++= r.get._1

      // Check label validity
      val label = r.get._2.get
      if (refLookup.contains(label)) {
        if (!refsInText.contains(label)) refsInText += (label -> refLookup(label))
        MakeRef(newText, label)
      } else
        MakeRef(newText, MakeInvalidRef(label))

      // Continue parsing
      r = parse(textAndPossibleRef, r.next)
    }
    newText ++= r.get._1

    curRefLookup = refLookup
    newText.toString
  }

  /**
   * Scans a text string for an optional "--> 1.0.2" suffix.
   * If found (and valid), the suffix is extracted and normalised.
   */
  private def parseTextLinkNext(text: String): (String, String) = {
    var (left, suffix) = (text, "")

    val p = parseAll(textWithLinkNext, text)
    if (p.successful) {
      val (actualText, labels) = p.get
      if (areAllLabelsValid(labels)) {
        val sortedLabels = TreeSet(labels: _*).mkString(", ")
        left = actualText.trim
        suffix = s"$ArrowGood $sortedLabels"
      }
    }

    left = ArrowRegex.replaceAllIn(left, ArrowBad)

    (left, suffix)
  }

  @inline private def areAllLabelsValid(labels: Seq[String]): Boolean = {
    val refLookup = refLookupProvider()
    labels.find(!refLookup.contains(_)).isEmpty
  }

  override def messageHandler = {
    case StepChangeMsg if refsInText.nonEmpty =>

      // Ignore changes if already processed
      val newRefLookup = refLookupProvider()
      if (newRefLookup != curRefLookup) {

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

        // Record ref lookup table so that we avoid re-processing when nothing upstream changes
        curRefLookup = newRefLookup
      }
  }

  private def updateTextJs(): JsCmd = JqId(id) ~> JqSetValue(text, false)
}