package shipreq.webapp.snippet.uce

import net.liftweb.http.js.JsCmd
import shipreq.webapp.lib.SnippetHelpers
import UseCaseEditor._

private [uce] trait RendererHelper {
  def state: State
  def modifyUC: UcModifier => JsCmd

  @inline final def uc = state.uc
  @inline final def ucNumber = uc.number
  @inline final def uch = uc.header
  @inline final def fields = uc.fields
  @inline final implicit def fieldValues = uc.fieldValues

  implicit def autoApplyModifier(m: UcModifier): JsCmd = modifyUC(m)
  implicit def autoApplyModifierFn(m: UcModifier): () => JsCmd = () => modifyUC(m)

  implicit def jsonFormats = SnippetHelpers.DefaultJsonFormat
}