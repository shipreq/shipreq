package com.beardedlogic.usecase.app

import net.liftweb.common._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import com.beardedlogic.usecase.model.{UseCaseSummary, UseCase}
import com.beardedlogic.usecase.lib.ExternalId

object AppSiteMap {

  val Login = Menu.i("Login") / "login"

  val Register1 = Menu(Loc("Register1", List("register"), "Register"))

  val Register2 = Menu.param[String]("Register2", "Register", token => Full(token), t => t) / "register" / * >> Hidden

  val UseCaseIndex = Menu.i("Use Cases") / "list"

  val UseCaseEditor = Menu.i("Use Case Editor") / "uce"

  val sitemap = SiteMap(Login, Register1, Register2, UseCaseIndex, UseCaseEditor)

  object Urls {
    // TODO viewUseCase() should be UseCaseEditor() and should use a Loc
    def viewUseCase(uc: UseCase): String = viewUseCase(ExternalId(uc.dataId))
    def viewUseCase(ucs: UseCaseSummary): String = viewUseCase(ucs.dataEid)
    def viewUseCase(dataEid: String): String = "/usecase/" + dataEid

    def completeRegistration(token: String) = "/register/" + token
  }
}