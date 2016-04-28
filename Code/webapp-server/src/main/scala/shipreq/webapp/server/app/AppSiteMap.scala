package shipreq.webapp.server
package app

import net.liftweb.common._
import net.liftweb.http.{RequestVar, S, Templates, RedirectResponse, LiftResponse, PlainTextResponse}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes.{Development, Test => TestMode}
import scala.xml.{Text, NodeSeq}
import scalaz.{Memo, Name, Need}
import scalaz.old.NonEmptyList

import shipreq.webapp.base.AppConsts
import AppConfig.BaseUrl
import shipreq.webapp.server.data._
import shipreq.webapp.server.lib.Misc
import shipreq.webapp.server.feature.{SessionStats, DiagnosticEndpoints, Navbar, NavbarElem}
import shipreq.webapp.server.security.{Permissions, Permission, Oshiro}
import shipreq.webapp.server.util.ExternalId
import Permission.RequestVarPermExt

object AppSiteMap {
  type PM[T] = Menu.ParamMenuable[T]

  // TODO SiteMap effects have a requested order. Right now they're unenforced.

  private implicit class RequestVarNExt[T](val rv: RequestVar[Name[T]]) extends AnyVal {
    def setByParam(pm: PM[T], desc: String) = setReqVar(rv, pm, desc)
  }

  private val landingPageTemplate = "landing_page"

  // -------------------------------------------------------------------------------------------------------------------
  // Menu.i(NAME_AND_TITLE) / PATH_FOR_URL_AND_TEMPLATE
  // Menu(Loc(NAME, PATH_FOR_URL_AND_TEMPLATE, TITLE))
  // Menu.param[PARAM_TYPE(S)](NAME, TITLE, URL_TO_PARAM, PARAM_TO_URL) / PATH_FOR_URL_AND_TEMPLATE

  val Home = pageWithStaticUrl("home", defaultTitle, "Home")(_ / "index"
    >> UseEitherTemplate(Oshiro.isAuthenticated, "loggedin/index")(landingPageTemplate)
  )

  val Land_BusinessCard = pageWithStaticUrl("land-bc", "")(_ / "bc" >> UseTemplate(landingPageTemplate))

  val About = pageWithStaticUrl("about", "About")(_ / "about")
  val TermsOfService = pageWithStaticUrl("terms", mkTitle("Terms of Service"), "Terms")(_ / "terms")
  val PrivacyPolicy = pageWithStaticUrl("privacy", mkTitle("Privacy Policy"), "Privacy")(_ / "privacy")

  val Login = pageWithStaticUrl("login", "Login")(_ / "login")
  val Logout = pageWithStaticUrl("logout", defaultTitle, "Logout")(_ / "logout" >> EarlyResponse(logout))

  val Register1 = pageWithStaticUrl("register1", mkTitle("Register"), "Register")(_ / "register")
  val Register2 = (
    Menu.param[String]("register2", "", i => Full(i), o => o) / "register" / *
    >> StaticTitle(mkTitle("Register"))
    >> Hidden >> UseTemplate("register2")
  )

  private def ResetPasswordTitle = mkTitle("Password Reset")
  val ResetPassword1 = pageWithStaticUrl("resetpw1", ResetPasswordTitle, "Forgotten Your Password?")(_ / "resetpw")
  val ResetPassword2 = (
    Menu.param[String]("resetpw2", "", i => Full(i), o => o) / "resetpw" / *
      >> StaticTitle(ResetPasswordTitle)
      >> Hidden >> UseTemplate("resetpw2")
    )

  val Project: PM[ProjectId] = (
    MenuWithIdParam(ProjectId.Extern)("project_spa") / "project" / * / **
    >> TitleFromProjectName
    >> AuthenticationRequired >> ProjectPermissionRequired
    >> UseTemplate("loggedin/project_spa")
    >> SetNavbarAndPerformEffects(Navbar.Home, Navbar.CurrentProject) {
        RequestVars.ProjectId.setByParam(Project, "Project --> ProjectId")
        RequestVars.Project.deriveFromProjectId()
      }
  )

  val AdminStats = pageWithStaticUrl("admin.stats", mkTitle("Stats"), "")(_ / "sir" / "stats" >> AdminOnly >> Hidden)

  // -------------------------------------------------------------------------------------------------------------------

  val AllProdPages: List[ConvertableToMenu] = List(
    Home, About, TermsOfService, PrivacyPolicy, Land_BusinessCard
    , Login, Logout, Register1, Register2, ResetPassword1, ResetPassword2
    , Project
    , AdminStats
  ) ++ DiagnosticEndpoints.Endpoints

  val sitemap = {
    import org.apache.shiro.authc.UsernamePasswordToken, org.apache.shiro.SecurityUtils.getSubject

    def autoLogin = Menu.i("x") / "x" >> EarlyResponse(() => {
      getSubject.login(new UsernamePasswordToken("devuser", "dev123123"))
      SessionStats.onLogin(S.session, Oshiro.loggedInUser.get)
      Full(redirectHomeResp)
      Full(RedirectResponse("/project/oLctx/table"))
    })

    def apiLogin = Menu.i("login.api") / "login.api" >> EarlyResponse(() => for {
      u <- S.param("user")
      p <- S.param("pass")
    } yield {
      getSubject.login(new UsernamePasswordToken(u, p))
      PlainTextResponse("OK")
    })

    val additionalPages: List[ConvertableToMenu] = Props.mode match {
      case Development => List(autoLogin)
      case TestMode    => List(apiLogin)
      case _           => List.empty
    }

    val pages = AllProdPages ++ additionalPages
    SiteMap(pages: _*)
  }

  // -------------------------------------------------------------------------------------------------------------------

  object Implicits {

    private def newUrlMemo: Memo[Loc[_], String] = Misc.newMemo(Equiv.reference)

    private val relUrlMemo = newUrlMemo(loc => {
      val s = loc.calcDefaultHref
      if (s.endsWith("/index"))
        if (s.length == 6)
          "/"
        else
          s.substring(0, s.length - 6)
      else
        s
    })

    private val absUrlMemo = newUrlMemo(loc => {
      loc.relativeUrl match {
        case "/" => BaseUrl
        case s   => BaseUrl + s
      }
    })

    implicit class LocExt(val loc: Loc[_]) extends AnyVal {
      def relativeUrl: String = relUrlMemo(loc)
      def absoluteUrl: String = absUrlMemo(loc)
    }

    implicit class MenuExt(val menu: Menu) extends AnyVal {
      def relativeUrl: String = menu.loc.relativeUrl
      def absoluteUrl: String = menu.loc.absoluteUrl
    }

    implicit class MenuableExt(val menu: Menu.Menuable) extends AnyVal {
      def relativeUrl: String = menu.loc.relativeUrl
      def absoluteUrl: String = menu.loc.absoluteUrl
    }

    implicit class ParamMenuableExt[T](val menu: Menu.ParamMenuable[T]) extends AnyVal {
      def relativeUrl(arg: T): String = menu.calcHref(arg)
      def absoluteUrl(arg: T): String = BaseUrl + relativeUrl(arg)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  import Implicits._

  def redirectHomeResp = RedirectResponse(Home.relativeUrl)

  def logout(): Box[LiftResponse] = {
    Oshiro.logout()
    SessionStats.onLogout(S.session)
    Full(redirectHomeResp)
  }

  @inline final def defaultTitle = AppConsts.appName

  @inline final def mkTitle(title: String): String = title match {
    case "" => defaultTitle
    case _  => s"$title | ${AppConsts.appName}"
  }

  private def StaticTitle[T](title: String) = {
    val titleXml: NodeSeq = Text(title)
    Title[T](_ => titleXml)
  }

  private def DynamicTitle[T](title: => String) =
    Title[T](_ => Text(title))

  private def pageWithStaticUrl(name: String, linkAndTitle: String)(f: Menu.PreMenu => Menu.Menuable): Menu.Menuable =
    pageWithStaticUrl(name, mkTitle(linkAndTitle), linkAndTitle)(f)

  private def pageWithStaticUrl(name: String, title: String, linkText: String)(f: Menu.PreMenu => Menu.Menuable): Menu.Menuable =
    f(Menu(name, linkText)) >> StaticTitle(title)

  private def projectName = RequestVars.Project.get.value.name

  private def TitleFromProjectName[T] = DynamicTitle[T](mkTitle(projectName))

  private def MenuWithIdParam[Id <: AnyRef](scheme: ExternalId.Scheme[Id])(name: String) =
    Menu.param[Id](name, "", scheme.parseB, scheme.toExternal(_).value)

  private def splitPath(path: String): List[String] =
    path.split("/").toList

  private def UseTemplate(path: String) = {
    val t = splitPath(path)
    // TODO is Templates reusable here?
    TemplateBox(() => Templates(t))
  }

  private def UseEitherTemplate(condB: => Boolean, pathB: String)(pathA: String) = {
    val a = splitPath(pathA)
    val b = splitPath(pathB)
    TemplateBox(() => Templates(if (condB) b else a))
  }

  private def AuthenticationRequired =
    If(() => Oshiro.isAuthenticated, () => RedirectResponse(Login.relativeUrl))

  private def PermissionRequired(checker: => Permission.Checker, failResp: LiftResponse = redirectHomeResp) =
    If(() => checker.isPass, () => failResp)

  private def ProjectPermissionRequired =
    PermissionRequired(Permissions.accessProject.using(project = RequestVars.Project.some))

  private def AdminOnly =
    Test(_ => Permissions.admin.using().isPass)

  private def buildNavbar(h: NavbarElem, t: NavbarElem*) = {
    val elems = NonEmptyList(h, t: _*)
    Navbar(elems.reverse)
  }

  private def SetNavbarAndPerformEffects(h: NavbarElem, t: NavbarElem*)(f: => Unit) = {
    val navbar = buildNavbar(h, t: _*)
    PerformEffects {f; RequestVars.Navbar.set(navbar)}
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
