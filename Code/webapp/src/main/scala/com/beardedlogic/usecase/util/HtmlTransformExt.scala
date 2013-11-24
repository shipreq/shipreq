package com.beardedlogic.usecase.util

import net.liftweb.http.{SHtml, S}
import net.liftweb.http.js.{JE, JsCmd}
import net.liftweb.util.{Helpers, CssSel}
import net.liftweb.util.Helpers.strToCssBindPromoter
import scala.xml.NodeSeq
import JsExt._

object HtmlTransformExt {

  final val PassThru = "dpp_recommends_this_oh_well" #> ""

  final def NoneS: Option[String] = None

  def removeClasses(cssSel: String)(x: String, xs: String*) =
    multiTransform(s"$cssSel [class!]" #> _)(x, xs: _*)

  def multiTransform(f: String => CssSel)(x: String, xs: String*) = {
    val fst: NodeSeq => NodeSeq = f(x)
    (fst /: xs.map(f))((a,b) => a andThen b)
  }

  def IfCssSel(cond: => Boolean)(expr: => CssSel): CssSel = if (cond) expr else PassThru

  /**
   * Sets the `onclick` attribute to submit this form via Lift + Ajax. No form IDs necessary.
   */
  final val ajaxSubmitThisFormOnClick = "* [onclick]" #> JqSubmitThisFormAndStop

  /**
   * Adds a hidden form values that ensures the given function is called when the form is submitted.
   */
  def callOnSubmit(fn: () => JsCmd) = {
    val fnName = S.fmapFunc(fn)(n => n)
    <input type="hidden" name={fnName} value="true" />
  }

  /**
   * Enhances a form so that it is reusable, meaning that the form can be duplicated on the client-side.
   *
   * @param onSubmitFn The function to call when the form is submitted.
   */
  def reusableAjaxForm(onSubmitFn: () => JsCmd): CssSel = (
    "form *+" #> callOnSubmit(onSubmitFn) &
    "form :submit" #> ajaxSubmitThisFormOnClick
  )

  /**
   * Transforms a button so that it submits its parent form when clicked, and invokes the given function when the
   * request hits the server.
   *
   * Copied from `SHtml.ajaxSubmit()` but modified so that it updates rather than replaces.
   */
  def ajaxSubmitOnClick(func: () => JsCmd): CssSel = {
    val funcName = "z" + Helpers.nextFuncName
    S.addFunctionMap(funcName, (func))
    "* [onclick]" #> s"liftAjax.lift_uriSuffix='$funcName=_';return true"
  }

  /**
   * Transforms an element so that it invokes an ajax function when clicked.
   *
   * Dismantled from `SHtml.ajaxButton()`.
   */
  def ajaxOnClick(func: () => JsCmd): CssSel = {
    val funcName = Helpers.nextFuncName
    S.addFunctionMap(funcName, (func))
    // val js = SHtml.makeAjaxCall(JE.Str(funcName + "=true")).toJsCmd + ";return false"
    val js = s"""liftAjax.lift_ajaxHandler("$funcName=true", null, null, null);return false"""
    "* [onclick]" #> js
  }
}
