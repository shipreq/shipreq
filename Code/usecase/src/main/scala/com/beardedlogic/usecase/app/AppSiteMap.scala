package com.beardedlogic.usecase.app

import java.lang.{Long => JLong}
import net.liftweb.common._
import net.liftweb.http.{Templates, RedirectResponse, LiftResponse}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes.{Development, Test => TestMode}
import org.apache.shiro.SecurityUtils

import AppConfig.BaseUrl
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.{ExternalId, ExternalIdConverter}

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
    >> Hidden >> UseTemplate("loggedin/project"))

  val UseCaseEditor = (MenuWithIdParam(ExternalId.UseCase)("uce", "Use Case Editor") / "usecase" / *
    >> Hidden >> UseTemplate("uce"))

  // -------------------------------------------------------------------------------------------------------------------

  val AllProdPages = List[ConvertableToMenu](
    Home, Login, Logout, Register1, Register2,
    Project, UseCaseEditor
  )

  val sitemap = {
    var pages = AllProdPages
    Props.mode match {
      case Development | TestMode => pages +:= Menu.i("Use Case Editor (demo)") / "uce"
      case _ =>
    }
    SiteMap(pages: _*)
  }

  def logout(): Box[LiftResponse] = {
    SecurityUtils.getSubject.logout()
    Full(RedirectResponse(HomeRelativeUrl))
  }

  private def MenuWithIdParam[Tag <: ExteralisableIdTag](eidGen: ExternalIdConverter[Tag])(name: String, linkText: Loc.LinkText[JLong @@ Tag]) =
    Menu.param[JLong @@ Tag](name, linkText, eidGen.parseB(_), eidGen.toExternal(_))

  private def UseTemplate(path: String) = TemplateBox(() => Templates(path.split("/").toList))

  object Implicits {

    implicit class MenuExt(val menu: Menu) extends AnyVal {
      def relativeUrl: String = menu.loc.calcDefaultHref
      def absoluteUrl: String = BaseUrl + relativeUrl
    }

    implicit class MenuableExt(val menu: Menu.Menuable) extends AnyVal {
      // TODO Gross hack for Home -> / instead of /index
      def relativeUrl: String = if (menu == Home) "/" else menu.loc.calcDefaultHref
      def absoluteUrl: String = BaseUrl + relativeUrl
    }

    implicit class ParamMenuableExt[T](val menu: Menu.ParamMenuable[T]) extends AnyVal {
      def relativeUrl(arg: T): String = menu.calcHref(arg)
      def absoluteUrl(arg: T): String = BaseUrl + relativeUrl(arg)
    }
  }
}
