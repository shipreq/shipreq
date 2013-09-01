package com.beardedlogic.usecase.snippet.uce

import net.liftweb.http.js.JsCmd
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.UseCase

private [uce] trait RendererHelper {
  def state: UseCaseEditor.State
  def updateUC: (UseCase => UcUpdateResult) => JsCmd

  @inline final def uc = state.uc
  @inline final def uch = uc.header
  @inline final def fields = uc.fields
  @inline final implicit def fieldValues = uc.fieldValues

  // % as in "mod(ify)"
  @inline final def %(f: UseCase => UcUpdateResult): JsCmd = updateUC(f)
  @inline final def =>%(f: UseCase => UcUpdateResult) = () => updateUC(f)
}
