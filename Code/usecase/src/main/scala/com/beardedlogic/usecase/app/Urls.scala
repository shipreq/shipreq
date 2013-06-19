package com.beardedlogic.usecase
package app

import lib.ExternalId
import model._

object Urls {

  def viewUseCase(uc: UseCase): String = viewUseCase(ExternalId(uc.value.dataId))
  def viewUseCase(ucs: UseCaseSummary): String = viewUseCase(ucs.dataEid)
  def viewUseCase(dataEid: String): String = "/usecase/" + dataEid
}
