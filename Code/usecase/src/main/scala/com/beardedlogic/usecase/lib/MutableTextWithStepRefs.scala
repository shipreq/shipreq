package com.beardedlogic.usecase.lib

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

  /**
   * Regex that matches a reference to a step.
   *
   * Note: The braces are required for the match to complete but are not part of the matched text.
   */
  val StepRefRegex = """(?<=\[).+?\..+?(?=\])""".r

  @inline def MakeRef(ref: String) = "[" + ref + "]"
}

/**
 * Encapsulates a String to provide the following functionality:
 * <ul>
 * <li>References to steps in the text are validated, invalid references are transformed to make the invalidity
 * obvious.</li>
 * <li>References to steps are updated when their labels change, and the updated text is pushed back to the client.</li>
 * </ul>
 */
class MutableTextWithStepRefs(val msgCentre: MessageCentre,
                              refLookupProvider: () => Map[String, String]
                               ) extends LiftActor {

  import MutableTextWithStepRefs._

  val id = nextFuncName

  private[lib] var curRefsInUse = Map.empty[String, String]
  private[lib] var curRefLookup = Map.empty[String, String]

  private[lib] var _text = ""

  def text = _text

  def text_=(newValue: String) {
    if (text != newValue) {
      _text = newValue
      findReferences()
    }
  }

  def init() {
    msgCentre.register(this)
    findReferences()
  }

  def renderTextarea = SHtml.ajaxTextarea(text, onTextChange _, "id" -> id)

  /**
   * Callback when the user changes the text.
   */
  def onTextChange(newValue: String): JsCmd = {
    text = newValue
    JsCmds.Noop
  }

  /**
   * Scans text for step references.
   */
  private def findReferences() {
    val refLookup = refLookupProvider()
    curRefsInUse = Map.empty
    for (m <- StepRefRegex.findAllMatchIn(text)) {
      val label = m.matched
      if (!curRefsInUse.contains(label) && refLookup.contains(label)) {
        curRefsInUse += (label -> refLookup(label))
      }
    }
    curRefLookup = refLookup
  }

  override def messageHandler = {
    case StepChangeMsg if curRefsInUse.nonEmpty =>

      // Ignore changes if already processed
      val newRefLookup = refLookupProvider()
      if (newRefLookup != curRefLookup) {

        // Update step references
        var newRefsInUse = Map.empty[String, String]
        var newText = text
        for ((oldLabel, id) <- curRefsInUse) {
          newRefLookup.get(id).map { newLabel =>
            newText = newText.replace(MakeRef(oldLabel), MakeRef(newLabel))
            newRefsInUse += (newLabel -> id)
          }
        }

        // Save and publish text changes
        if (newText != text) {
          msgCentre ! PushToClient(JqId(id) ~> JqSetValue(newText, false))
          _text = newText
          curRefsInUse = newRefsInUse
        }

        // Record ref lookup table so that we avoid re-processing when nothing upstream changes
        curRefLookup = newRefLookup
      }
  }
}