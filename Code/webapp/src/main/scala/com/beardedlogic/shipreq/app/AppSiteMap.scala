package com.beardedlogic.shipreq
package app

import net.liftweb.common._
import net.liftweb.http.{RequestVar, S, Templates, RedirectResponse, LiftResponse, PlainTextResponse}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes.{Development, Test => TestMode}
import scalaz.{Name, Need, NonEmptyList}

import AppConfig.BaseUrl
import lib.Types._
import feature.{DiagnosticEndpoints, ExternalId, ExternalIdConverter, Navbar, NavbarElem}
import security.{Permissions, Permission, Oshiro}
import Permission.RequestVarPermExt

object AppSiteMap {
  type PM[T] = Menu.ParamMenuable[T]

  // TODO SiteMap effects have a requested order. Right now they're unenforced.

  private implicit class RequestVarNExt[T](val rv: RequestVar[Name[T]]) extends AnyVal {
    def setByParam(pm: PM[T], desc: String) = setReqVar(rv, pm, desc)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Menu.i(NAME_AND_TITLE) / PATH_FOR_URL_AND_TEMPLATE
  // Menu(Loc(NAME, PATH_FOR_URL_AND_TEMPLATE, TITLE))
  // Menu.param[PARAM_TYPE(S)](NAME, TITLE, URL_TO_PARAM, PARAM_TO_URL) / PATH_FOR_URL_AND_TEMPLATE

  val HomeRelativeUrl = "/"

  val Home = Menu.i("Home") / "index"

  val Login = Menu.i("Login") / "login"

  val Logout = Menu.i("Logout") / "logout" >> EarlyResponse(logout)

  val Register1 = Menu(Loc("Register1", List("register"), "Register"))

  val Register2 = (
    Menu.param[String]("Register2", "_", i => Full(i), o => o) / "register" / *
    >> Hidden >> UseTemplate("register2"))

  val Project: PM[ProjectId] = (
    MenuWithIdParam(ExternalId.Project)("project") / "project" / *
    >> AuthenticationRequired >> ProjectPermissionRequired
    >> UseTemplate("loggedin/project")
    >> UsesNavbar(Navbar.Home, Navbar.CurrentProject)
    >> PerformEffects {
        RequestVars.ProjectId.setByParam(Project, "Project --> ProjectId")
        RequestVars.Project.deriveFromProjectId()
      }
  )

  val ShareCreate: PM[ProjectId] = (
    MenuWithIdParam(ExternalId.Project)("share-create") / "project" / * / "share"
    >> AuthenticationRequired >> ProjectPermissionRequired
    >> UseTemplate("loggedin/share-create")
    >> UsesNavbar(Navbar.Home, Navbar.CurrentProject, Navbar.StaticText("Share Use Cases"))
    >> PerformEffects {
        RequestVars.ProjectId.setByParam(ShareCreate, "ShareCreate --> ProjectId")
        RequestVars.Project.deriveFromProjectId()
      }
  )

  val ShareEdit: PM[ShareUrlToken] = (
    Menu.param[ShareUrlToken]("share-edit", "_", i => Full(i.tag), o => o) / "share" / * / "edit"
    >> AuthenticationRequired
    >> PermissionRequired(Permissions.editShare.using(project = RequestVars.Project.some, share = RequestVars.Share.some))
    >> UseTemplate("loggedin/share-edit")
    >> UsesNavbar(Navbar.Home, Navbar.CurrentProject, Navbar.StaticText("Edit Share"))
    >> PerformEffects {
        val token = Need(ShareEdit.currentValue.get)
        RequestVars.deriveShareAndProjectFromShareUrlToken(token)
      }
  )

  val ShareView: PM[ShareUrlToken] = (
    Menu.param[ShareUrlToken]("share-view", "_", i => Full(i.tag), o => o) / "share" / *
    >> UseTemplate("share-view")
  )

  val ReadOwnUcs: PM[ProjectId] = (
    MenuWithIdParam(ExternalId.Project)("readOwnUcs") / "project" / * / "read"
    >> AuthenticationRequired >> ProjectPermissionRequired
    >> UseTemplate("loggedin/read_own_ucs")
    >> UsesNavbar(Navbar.Home, Navbar.CurrentProject, Navbar.StaticText("Use Cases"))
    >> PerformEffects {
        RequestVars.ProjectId.setByParam(ReadOwnUcs, "ReadOwnUcs --> ProjectId")
        RequestVars.Project.deriveFromProjectId()
      }
  )

  val UseCaseEditor: PM[UseCaseIdentId] = (
    MenuWithIdParam(ExternalId.UseCase)("uce") / "usecase" / *
    >> AuthenticationRequired >> ProjectPermissionRequired
    >> UseTemplate("loggedin/uceditor")
    >> UsesNavbar(Navbar.Home, Navbar.CurrentProject, Navbar.UseCaseDropdown)
    >> PerformEffects {
        RequestVars.UseCaseId.setByParam(UseCaseEditor, "UseCaseEditor --> SoleUseCaseId")
        RequestVars.Project.deriveFromUseCaseId()
        RequestVars.ProjectId.deriveFromProject()
      }
  )

  // -------------------------------------------------------------------------------------------------------------------

  val AllProdPages: List[ConvertableToMenu] = List(
    Home, Login, Logout, Register1, Register2,
    Project, UseCaseEditor, ReadOwnUcs, ShareCreate, ShareEdit, ShareView
  ) ++ DiagnosticEndpoints.Endpoints

  val sitemap = {
    import org.apache.shiro.authc.UsernamePasswordToken, org.apache.shiro.SecurityUtils.getSubject

    def anonUce = (Menu.i("Use Case Editor Demo") / "uce"
      >> UseTemplate("loggedin/uceditor")
      >> UsesNavbar(Navbar.Home, Navbar.StaticText("Use Case Editor Demo"))
    )

    def autoLogin = Menu.i("x") / "x" >> EarlyResponse(() => {
      getSubject.login(new UsernamePasswordToken("golly", "asdasd123"))
      Full(redirectHomeResp)
      // Full(RedirectResponse("/project/cUZ0/share"))
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

  private def MenuWithIdParam[Tag <: IsExteralisableId](eidGen: ExternalIdConverter[Tag])(name: String) =
    Menu.param[JLong @@ Tag](name, "_", eidGen.parseB(_), eidGen.toExternal(_))

  private def UseTemplate(path: String) = TemplateBox(() => Templates(path.split("/").toList))

  private def AuthenticationRequired =
    If(() => Oshiro.isAuthenticated, () => RedirectResponse(Login.relativeUrl))

  private def PermissionRequired(checker: => Permission.Checker, failResp: LiftResponse = redirectHomeResp) =
    If(() => checker.isPass, () => failResp)

  private def ProjectPermissionRequired =
    PermissionRequired(Permissions.accessProject.using(project = RequestVars.Project.some))

  private def UsesNavbar(h: NavbarElem, t: NavbarElem*) = {
    val elems = NonEmptyList(h, t: _*)
    val navbar = Navbar(elems.reverse)
    PerformEffects(RequestVars.Navbar.set(navbar))
  }

  private def PerformEffects(f: => Unit) = Test(_ => {f; true})

  private def setReqVar[T](v: RequestVar[Name[T]], m: PM[T], desc: String): Unit =
    v.set(Need(
      m.currentValue match {
        case Full(t) => t
        case b@_ => throw new IllegalStateException(s"RequestVar get failed, menu param unavailable. ($desc: $b)")
      }
    ))
}
