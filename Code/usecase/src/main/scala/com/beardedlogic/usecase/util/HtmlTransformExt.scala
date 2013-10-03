package com.beardedlogic.usecase.util

import net.liftweb.http.S
import net.liftweb.http.js.JsCmd
import net.liftweb.util.{Helpers, CssSel}
import net.liftweb.util.Helpers.strToCssBindPromoter
import JsExt._

object HtmlTransformExt {

  final val PassThru = "dpp_recommends_this_oh_well" #> ""

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
}
