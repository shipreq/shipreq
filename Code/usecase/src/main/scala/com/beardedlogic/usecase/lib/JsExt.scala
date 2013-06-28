package com.beardedlogic.usecase.lib

import net.liftweb.http.js.{ JsCmd, JsCmds, JsExp, JsMember }
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

/**
 * Custom Javascript and JQuery extensions.
 *
 * @since 1/5/2013
 */
object JsExt {

  trait JqDurationExpr {
    def toJsArg: Option[String]
    @inline final def asOnlyArg = toJsArg getOrElse ""
    @inline final def asOptionalNonLastArg = toJsArg map (_ + ",") getOrElse ""
  }
  object DefaultDuration extends JqDurationExpr { override def toJsArg = None }
  object Fast extends JqDurationExpr { override def toJsArg = Some("'fast'") }
  object Slow extends JqDurationExpr { override def toJsArg = Some("'slow'") }
  case class JqDuration(milliseconds: Int) extends JqDurationExpr { override def toJsArg = Some(milliseconds.toString) }

  implicit class JqIntExt(val i: Int) extends AnyVal {
    def ms = JqDuration(i)
  }

  // -------------------------------------------------------------------------------------------------------------------

  /** A JQuery query for an element based on the id of the element. ie. `$('#id')`*/
  case class JqId(id: String) extends JsExp {
    override def toJsCmd = s"$$('#${id}')"
  }

  /** A JQuery query for a given expression. ie. `$(expr)` */
  case class JqExpr(expr: String) extends JsExp {
	  override def toJsCmd = s"$$('${expr}')"
  }

  /**
   * Insert content, specified by the parameter, after each element in the set of matched elements.
   *
   * @see http://api.jquery.com/after/
   */
  case class JqAfter(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd =
      "after(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  /**
   * Insert content, specified by the parameter, before each element in the set of matched elements.
   *
   * @see http://api.jquery.com/before/
   */
  case class JqBefore(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd =
      "before(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  /**
   * Insert content, specified by the parameter, to the end of each element in the set of matched elements.
   *
   * @see http://api.jquery.com/append/
   */
  case class JqAppend(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd =
      "append(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  /**
   * Insert content, specified by the parameter, to the beginning of each element in the set of matched elements.
   *
   * @see http://api.jquery.com/prepend/
   */
  case class JqPrepend(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd =
      "prepend(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  object JqHide extends JsExp with JsMember { override val toJsCmd = "hide()" }

  def JqSlideDown(duration: JqDurationExpr = DefaultDuration) = new JsExp with JsMember {
    override val toJsCmd = "slideDown(" + duration.asOnlyArg + ")"
  }

  def JqFadeIn(duration: JqDurationExpr = DefaultDuration) = new JsExp with JsMember {
    override val toJsCmd = "fadeIn(" + duration.asOnlyArg + ")"
  }

  /**
   * Sets the value of a text element or a textarea.
   *
   * @param newValue The new value of the target element(s).
   * @param callLift For ajax elements, whether the client should notify us, the server, the same way it would if the
   *                 user made a manual change.
   */
  def JqSetValue(newValue: String, callLift: Boolean) = new JsExp with JsMember {
    override val toJsCmd = "val(" + newValue.encJs + (if (callLift) ").blur()" else ")")
  }

  // Unlike everything else here, this doesn't extend JqId() ~>. This is a full cmd.
  // For now I'll just leave off the Jq prefix...
  def FadeOutThen(idExpr: JsExp, duration: JqDurationExpr = DefaultDuration)(onComplete: JsExp => JsCmd): JsCmd =
    JsCmds.Run(s"${idExpr.toJsCmd}.fadeOut(${duration.asOptionalNonLastArg}function(){${onComplete(idExpr).toJsCmd}});")

  /** Gives an element keyboard focus. */
  object JqFocus extends JsExp with JsMember {override val toJsCmd = "focus()"}

  /** Selects all text within an element. */
  object JqSelect extends JsExp with JsMember {override val toJsCmd = "select()"}

  /** Display or hide the matched elements. */
  object JqToggle extends JsExp with JsMember {override val toJsCmd = "toggle()"}

  /** Get the descendants of each element in the current set of matched elements, filtered by a selector. */
  def JqFind(selector: String) = new JsExp with JsMember {override val toJsCmd = s"find(${selector.encJs})"}
}
