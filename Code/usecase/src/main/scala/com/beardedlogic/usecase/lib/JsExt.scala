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

  val DurationFast = 200
  val DurationDefault = 400
  val DurationSlow = 600

  /**
   * A JQuery query for an element based on the id of the element
   */
  case class JqId(id: String) extends JsExp {
    override def toJsCmd = s"$$('#${id}')"
  }

  /**
   * A JQuery query for a given expression.
   */
  case class JqExpr(expr: String) extends JsExp {
	  override def toJsCmd = s"$$('${expr}')"
  }

  /**
   * See http://api.jquery.com/after/
   */
  case class JqAfter(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd =
      "after(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }
  /**
   * See http://api.jquery.com/before/
   */
  case class JqBefore(content: NodeSeq) extends JsExp with JsMember {
	  override val toJsCmd =
			  "before(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  object JqSlideDown extends JsExp with JsMember { override val toJsCmd = "slideDown()" }
  object JqSlideDownSlow extends JsExp with JsMember { override val toJsCmd = "slideDown('slow')" }
  object JqSlideDownFast extends JsExp with JsMember { override val toJsCmd = "slideDown('fast')" }
  def JqSlideDown(duration: Int): JsExp with JsMember =
    new JsExp with JsMember { override val toJsCmd = s"slideDown(${duration})" }

  object JqHide extends JsExp with JsMember { override val toJsCmd = "hide()" }

  def FadeOut(idExpr: JsExp, duration: Int = DurationDefault)(onComplete: JsExp => JsCmd): JsCmd =
    JsCmds.Run(s"${idExpr.toJsCmd}.fadeOut(${duration},function(){${onComplete(idExpr).toJsCmd}});")

  /**
   * Sets the value of a text element or a textarea.
   *
   * @param newValue The new value of the target element(s).
   * @param callLift For ajax elements, whether the client should notify us, the server, the same way it would if the
   *                 user made a manual change.
   */
  case class JqSetValue(newValue: String, callLift: Boolean) extends JsExp with JsMember {
    override val toJsCmd =
      "val(" + newValue.encJs + (if (callLift) ").blur()" else ")")
  }
}