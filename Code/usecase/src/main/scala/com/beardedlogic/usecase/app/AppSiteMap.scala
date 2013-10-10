package com.beardedlogic.usecase
package app

import net.liftweb.common._
import net.liftweb.http.{S, Templates, RedirectResponse, LiftResponse, PlainTextResponse}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes.{Development, Test => TestMode}

import AppConfig.BaseUrl
import lib.Types._
import lib.{ExternalId, ExternalIdConverter}
import security.{PermissionCheck, Oshiro}

object AppSiteMap {

  // Menu.i(NAME_AND_TITLE) / PATH_FOR_URL_AND_TEMPLATE
  // Menu(Loc(NAME, PATH_FOR_URL_AND_TEMPLATE, TITLE))
  // Menu.param[PARAM_TYPE(S)](NAME, TITLE, URL_TO_PARAM, PARAM_TO_URL) / PATH_FOR_URL_AND_TEMPLATE

  val HomeRelativeUrl = "/"

  val Home = Menu.i("Home") / "index"

  val Login = Menu.i("Login") / "login"

  val Logout = Menu.i("Logout") / "logout" >> EarlyResponse(logout)

  val Register1 = Menu(Loc("Register1", List("register"), "Register"))

  val Register2 = (Menu.param[String]("Register2", "Register", token => Full(token), t => t) / "register" / *
    >> Hidden >> UseTemplate("register2"))

  val Project = (MenuWithIdParam(ExternalId.Project)("project", "Project") / "project" / *
    >> AuthenticationRequired >> ProjectPermissionRequired
    >> UseTemplate("loggedin/project"))

  val UseCaseEditor = (MenuWithIdParam(ExternalId.UseCase)("uce", "Use Case Editor") / "usecase" / *
    >> AuthenticationRequired >> ProjectPermissionRequired
    >> UseTemplate("uce"))

  // -------------------------------------------------------------------------------------------------------------------

  val AllProdPages = List[ConvertableToMenu](
    Home, Login, Logout, Register1, Register2,
    Project, UseCaseEditor
  )

  val sitemap = {
    import org.apache.shiro.authc.UsernamePasswordToken, org.apache.shiro.SecurityUtils.getSubject

    def anonUce = Menu.i("Use Case Editor (demo)") / "uce"

    def autoLogin = Menu.i("x") / "x" >> EarlyResponse(() => {
      getSubject.login(new UsernamePasswordToken("golly", "asdasd123"))
      Full(redirectHomeResp)
    })

    def apiLogin = Menu.i("login.api") / "login.api" >> EarlyResponse(() => for {
      u <- S.param("user")
      p <- S.param("pass")
    } yield {
      getSubject.login(new UsernamePasswordToken(u, p))
      PlainTextResponse("OK")
    })

    val additionalPages: List[ConvertableToMenu] = Props.mode match {
      case Development => List(anonUce, autoLogin)
      case TestMode    => List(anonUce, apiLogin)
      case _           => List.empty
    }

    val pages = AllProdPages ++ additionalPages
    SiteMap(pages: _*)
  }

  // -------------------------------------------------------------------------------------------------------------------

  object Implicits {

    implicit class MenuExt(val menu: Menu) extends AnyVal {
      def relativeUrl: String = menu.loc.calcDefaultHref
      def absoluteUrl: String = BaseUrl + relativeUrl
    }

    implicit class MenuableExt(val menu: Menu.Menuable) extends AnyVal {
      def relativeUrl: String = if (menu eq Home) "/" else menu.loc.calcDefaultHref
      def absoluteUrl: String = BaseUrl + relativeUrl
    }

    implicit class ParamMenuableExt[T](val menu: Menu.ParamMenuable[T]) extends AnyVal {
      def relativeUrl(arg: T): String = menu.calcHref(arg)
      def absoluteUrl(arg: T): String = BaseUrl + relativeUrl(arg)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  import Implicits._

  def redirectHomeResp = RedirectResponse(HomeRelativeUrl)

  def logout(): Box[LiftResponse] = {
    Oshiro.logout()
    Full(redirectHomeResp)
  }

  private def MenuWithIdParam[Tag <: IsExteralisableId](eidGen: ExternalIdConverter[Tag])(name: String, linkText: Loc.LinkText[JLong @@ Tag]) =
    Menu.param[JLong @@ Tag](name, linkText, eidGen.parseB(_), eidGen.toExternal(_))

  private def UseTemplate(path: String) = TemplateBox(() => Templates(path.split("/").toList))

  private def AuthenticationRequired =
    If(() => Oshiro.isAuthenticated, () => RedirectResponse(Login.relativeUrl))

  private def PermissionRequired(check: PermissionCheck => PermissionCheck, failResp: LiftResponse = redirectHomeResp) =
    If(() => check(PermissionCheck.userCan).expect, () => failResp)

  private def ProjectPermissionRequired =
    PermissionRequired(_.readAndUpdate(RequestVars.SoleProject))
}
