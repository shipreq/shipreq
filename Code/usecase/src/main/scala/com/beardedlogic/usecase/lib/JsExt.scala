package com.beardedlogic.usecase.lib

import net.liftweb.http.js.{ JsCmd, JsCmds, JsExp, JsMember }
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
    override def toJsCmd = s"jQuery('#${id}')"
  }

  /**
   * See http://api.jquery.com/after/
   */
  case class JqAfter(content: NodeSeq) extends JsExp with JsMember {
    override val toJsCmd =
      "after(" + fixHtmlFunc("inline", content) { str => str } + ")"
  }

  object JqSlideDown extends JsExp with JsMember { override val toJsCmd = "slideDown()" }
  object JqSlideDownSlow extends JsExp with JsMember { override val toJsCmd = "slideDown('slow')" }
  object JqSlideDownFast extends JsExp with JsMember { override val toJsCmd = "slideDown('fast')" }
  def JqSlideDown(duration: Int): JsExp with JsMember =
    new JsExp with JsMember { override val toJsCmd = s"slideDown(${duration})" }

  object JqHide extends JsExp with JsMember { override val toJsCmd = "hide()" }

  def FadeOut(idExpr: JsExp, duration: Int = DurationDefault)(onComplete: JsExp => JsCmd): JsCmd =
    JsCmds.Run(s"${idExpr.toJsCmd}.fadeOut(${duration},function(){${onComplete(idExpr).toJsCmd}});")
}