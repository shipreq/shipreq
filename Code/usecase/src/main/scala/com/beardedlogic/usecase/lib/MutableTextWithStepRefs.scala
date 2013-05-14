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
  val StepRefRegex = """(?<=\[)\s*?[A-Za-z0-9][A-Za-z0-9\s.]+?(?=\])""".r

  /**
   * Whitespace are dots is removed. This regex matches a dot with optional whitespace on either side.
   */
  val DotWithWhitespaceRegex = """\s*\.\s*""".r

  @inline def MakeRef(ref: String) = "[" + ref + "]"

  @inline def InvalidStepRef(label: String) = label + "?"

  @inline def NormaliseStepRef(label: String) = DotWithWhitespaceRegex.replaceAllIn(label.trim, ".")

  val DeletedRef = MakeRef("DELETED")
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
  private def parseText(text: String): String = {
    val refLookup = refLookupProvider()
    curRefsInUse = Map.empty
    val newText = StepRefRegex.replaceAllIn(text, m => {

      // Inspect ref
      val rawLabel = m.matched
      if (rawLabel.indexOf('.') == -1)
        rawLabel // ignore refs without dots

      else {
        val label = NormaliseStepRef(m.matched)
        if (refLookup.contains(label)) {

          // Match found
          if (!curRefsInUse.contains(label)) curRefsInUse += (label -> refLookup(label))
          label
        } else
          InvalidStepRef(label)
      }
    })
    curRefLookup = refLookup
    newText
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

          // Lookup each existing reference
          newRefLookup.get(id).map { newLabel =>
            if (oldLabel != newLabel)
              newText = newText.replace(MakeRef(oldLabel), MakeRef(newLabel))
            if (!newRefsInUse.contains(newLabel)) newRefsInUse += (newLabel -> id)
          } orElse {
            newText = newText.replace(MakeRef(oldLabel), DeletedRef)
            None
          }
        }

        // Save and publish text changes
        if (newText != text) {
          _text = newText
          curRefsInUse = newRefsInUse
          msgCentre ! PushToClient(updateTextJs)
        }

        // Record ref lookup table so that we avoid re-processing when nothing upstream changes
        curRefLookup = newRefLookup
      }
  }

  private def updateTextJs(): JsCmd = JqId(id) ~> JqSetValue(text, false)
}