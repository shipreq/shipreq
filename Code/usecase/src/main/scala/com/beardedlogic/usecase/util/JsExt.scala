package com.beardedlogic.usecase.util

import net.liftweb.common.Empty
import net.liftweb.http.js._
import net.liftweb.http.{JsContext, SHtml}
import net.liftweb.json.Formats
import net.liftweb.json.Serialization.{write => jsonWrite}
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import scalaz.Cord

/**
 * Custom Javascript and JQuery extensions.
 *
 * @since 1/5/2013
 */
object JsExt {

  trait JqDurationExpr {
    def toJsArg: Option[String]
    @inline final def asOnlyArg = toJsArg getOrElse ""
    @inline final def asOptionalLastArg = toJsArg map ("," + _) getOrElse ""
    @inline final def asOptionalNonLastArg = toJsArg map (_ + ",") getOrElse ""
  }
  object DefaultDuration extends JqDurationExpr { override def toJsArg = None }
  object Fast extends JqDurationExpr { override def toJsArg = Some("'fast'") }
  object Slow extends JqDurationExpr { override def toJsArg = Some("'slow'") }
  case class JqDuration(milliseconds: Int) extends JqDurationExpr { override def toJsArg = Some(milliseconds.toString) }

  implicit class JqIntExt(val i: Int) extends AnyVal {
    def ms = JqDuration(i)
  }

  trait JsMethod extends JsExp with JsMember

  implicit def cord2jsExp(c: Cord): JsExp = JE.Str(c.toString)

  /**
   * Many jQuery functions allow the last argument to be a callback that is invoked when the function completes.
   * This trait marks functions with that behaviour and allows a callback to be added on.
   */
  trait JsMethodWithOptionalOnCompleteCallback extends JsMethod {
    def andThen(onComplete: => JsCmd) = {
      val cmd1 = toJsCmd
      assume(cmd1.last == ')', s"JS should end with right parenthesis. Got: $cmd1")
      assume(cmd1.length > 2, s"JS should be at least 3 chars in length. Got: $cmd1")
      val comma = if (cmd1(cmd1.length - 2) == '(') "" else ","
      val js = cmd1.substring(0, cmd1.length - 1) + comma + s"function(){${onComplete.toJsCmd}})"
      new JsMethod {override val toJsCmd = js}
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  /** A JQuery query for an element based on the id of the element. ie. `$('#id')`*/
  case class JqId(id: String) extends JsExp {
    override def toJsCmd = s"$$('#${id}')"
  }

  /** A JQuery query for a given expression. ie. `$(expr)` */
  class JqExpr private(expr: String) extends JsExp {
    override def toJsCmd = s"$$(${expr})"
  }
  object JqExpr extends HtmlFixer {
    def apply(expr: String): JqExpr = new JqExpr(expr.encJs)
    def apply(content: NodeSeq): JqExpr = new JqExpr(fixHtmlFunc("inline", content) { str => str })
  }

  /**
   * Set the HTML contents of each element in the set of matched elements.
   *
   * @see http://api.jquery.com/html/
   */
  case class JqHtml(content: NodeSeq) extends JsMethod {
    override val toJsCmd =
      "html(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  /**
   * Insert content, specified by the parameter, after each element in the set of matched elements.
   *
   * @see http://api.jquery.com/after/
   */
  case class JqAfter(content: NodeSeq) extends JsMethod {
    override val toJsCmd =
      "after(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  /**
   * Insert content, specified by the parameter, before each element in the set of matched elements.
   *
   * @see http://api.jquery.com/before/
   */
  case class JqBefore(content: NodeSeq) extends JsMethod {
    override val toJsCmd =
      "before(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  /**
   * Insert content, specified by the parameter, to the end of each element in the set of matched elements.
   *
   * @see http://api.jquery.com/append/
   */
  case class JqAppend(content: NodeSeq) extends JsMethod {
    override val toJsCmd =
      "append(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  /**
   * Insert content, specified by the parameter, to the beginning of each element in the set of matched elements.
   *
   * @see http://api.jquery.com/prepend/
   */
  case class JqPrepend(content: NodeSeq) extends JsMethod {
    override val toJsCmd =
      "prepend(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  /**
   * Insert every element in the set of matched elements to the end of the target.
   *
   * Example: `JqExpr(nodeSeq) ~> JqAppendTo("#notices")`
   *
   * @param expr The elements to receive the new content.
   * @see http://api.jquery.com/appendTo/
   */
  def JqAppendTo(expr: String) = new JsMethod {override val toJsCmd = s"appendTo(${expr.encJs})"}

  /**
   * Insert every element in the set of matched elements to the end of the target.
   *
   * Example: `JqExpr(nodeSeq) ~> JqPrependTo("#notices")`
   *
   * @param expr The elements to receive the new content.
   * @see http://api.jquery.com/prependTo/
   */
  def JqPrependTo(expr: String) = new JsMethod {override val toJsCmd = s"prependTo(${expr.encJs})"}

  def JqSlideDown(duration: JqDurationExpr = DefaultDuration) = new JsMethodWithOptionalOnCompleteCallback {
    override val toJsCmd = "slideDown(" + duration.asOnlyArg + ")"
  }

  def JqFadeIn(duration: JqDurationExpr = DefaultDuration) = new JsMethodWithOptionalOnCompleteCallback {
    override val toJsCmd = "fadeIn(" + duration.asOnlyArg + ")"
  }

  /**
   * Sets the value of a text element or a textarea.
   *
   * @param newValue The new value of the target element(s).
   */
  def JqSetValue(newValue: String) = new JsMethod {
    override val toJsCmd = JqSetValueJs(newValue)
  }
  def JqSetValueJs(newValue: String) = "val(" + newValue.encJs + ")"

  /**
   * Sets the value of a textarea.
   *
   * @param newValue The new value of the target element(s).
   */
  def JqSetTextarea(newValue: String) = new JsMethod {
    override val toJsCmd = JqSetValueJs(newValue) + ".trigger('autosize.resize')"
  }

  // Unlike everything else here, this doesn't extend JqId() ~>. This is a full cmd.
  // For now I'll just leave off the Jq prefix...
  def FadeOutThen(idExpr: JsExp, duration: JqDurationExpr = DefaultDuration)(onComplete: JsExp => JsCmd): JsCmd =
    JsCmds.Run(s"${idExpr.toJsCmd}.fadeOut(${duration.asOptionalNonLastArg}function(){${onComplete(idExpr).toJsCmd}});")

  /** Gives an element keyboard focus. */
  object JqFocus extends JsMethod {override val toJsCmd = "focus()"}

  /** Selects all text within an element. */
  object JqSelect extends JsMethod {override val toJsCmd = "select()"}

  /** Display or hide the matched elements. */
  object JqToggle extends JsMethod {override val toJsCmd = "toggle()"}

  object JqHide extends JsMethod { override val toJsCmd = "hide()" }

  object JqRemove extends JsMethod {override val toJsCmd = "remove()"}

  /** Get the descendants of each element in the current set of matched elements, filtered by a selector. */
  def JqFind(selector: String) = new JsMethod {override val toJsCmd = s"find(${selector.encJs})"}

  /** Causes matches elements to flash. */
  def JqHighlight(duration: JqDurationExpr = DefaultDuration) = new JsMethodWithOptionalOnCompleteCallback {
    override val toJsCmd = "effect('highlight'" + duration.asOptionalLastArg + ")"
  }

  /** Selects the form containing `this`. */
  object JqThisForm extends JsExp {override def toJsCmd = "$(this).parents('form')"}

  object JqSerialiseThisForm extends JsExp {override def toJsCmd = JqThisForm.toJsCmd + ".serialize()"}

  /** Submits this form via Lift (using Ajax). */
  final val JqSubmitThisForm = SHtml.makeAjaxCall(JqSerialiseThisForm, new JsContext(Empty, Empty))

  /**
   * Meant to be used as an onClick attribute.
   * Submits this form via Lift (using Ajax), then returns `false` to stop the form POSTing.
   */
  final val JqSubmitThisFormAndStop = JqSubmitThisForm.toJsCmd + "; return false"

  /**
   * Invokes a JavaScript trigger.
   *
   * The JavaScript should define triggers as follows:
   * {{{
   * $(document).on('my-trigger-name', function(event, data) {
   *   // Do something with data
   * });
   * }}}
   */
  abstract class JsTrigger(triggerName: String) {
    private val jsFirstHalf = s"$$(document).trigger(${triggerName.encJs},"
    protected def go(data: JsExp): JsCmd = JsCmds.Run(s"${jsFirstHalf}${data.toJsCmd})")
  }

  /** Invokes a JavaScript trigger with JSON data. */
  case class JsJsonTrigger[T <: AnyRef](triggerName: String) extends JsTrigger(triggerName) {
    def trigger(data: T)(implicit jsonFormats: Formats) = go(JE.JsRaw(jsonWrite(data)))
  }

  /** Invokes a JavaScript trigger with textual data. */
  case class JsTextTrigger(triggerName: String) extends JsTrigger(triggerName) {
    def trigger(text: String): JsCmd = go(JE.Str(text))
    def trigger(text: Cord): JsCmd = go(text)
  }

  /** Sets a global JS variable to the value denoted by a given JS expression. */
  def JsSetGlobalVar(varName: String, valueExpr: JsExp) = new JsCmd {
    override def toJsCmd = s"$varName=${valueExpr.toJsCmd}"
  }

  object JqEnable extends JsMethod {override val toJsCmd = "removeAttr('disabled')"}

  object JqDisable extends JsMethod {override val toJsCmd = "attr('disabled','disabled')"}
}
