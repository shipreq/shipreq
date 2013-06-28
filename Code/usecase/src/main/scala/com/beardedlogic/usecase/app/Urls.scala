package com.beardedlogic.usecase
package app

import AppConfig.BaseUrl
import lib.ExternalId
import model._

object Urls {

  def viewUseCase(uc: UseCase): String = viewUseCase(ExternalId(uc.dataId))
  def viewUseCase(ucs: UseCaseSummary): String = viewUseCase(ucs.dataEid)
  def viewUseCase(dataEid: String): String = "/usecase/" + dataEid

  def completeRegistration(token: String) = "/register/" + token
  final val login = "/login"
}
