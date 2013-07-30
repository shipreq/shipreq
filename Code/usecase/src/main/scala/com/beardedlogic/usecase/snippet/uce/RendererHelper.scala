package com.beardedlogic.usecase.snippet.uce

import net.liftweb.http.js.JsCmd
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.UseCase
import com.beardedlogic.usecase.model.DAO

private [uce] trait RendererHelper {
  val uce: UseCaseEditor

  @inline final def textFieldIds = uce.textFieldIds
  @inline final def state = uce.state
  @inline final def uch = state.uc.header
  @inline final def fields = state.uc.fields
  @inline final implicit def fieldValues = state.uc.fieldValues

  // % as in "mod(ify)"
  @inline final def %(f: UseCase => UcUpdateResult): JsCmd = uce.update(f)
  @inline final def =>%(f: UseCase => UcUpdateResult) = () => uce.update(f)

  @inline final def withDao[R](f: DAO => R): R = uce.daoProvider.withTransaction(f)
  @inline final def daoCallback[R](f: DAO => R) = () => withDao(f)
}
