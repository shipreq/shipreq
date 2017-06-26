package shipreq.webapp.server.app

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util.Props
import net.liftweb.util.Props.RunModes.{Development, Test => TestMode}
import scala.xml.{NodeSeq, Text}
import scalaz.{Name, Need}
import shipreq.base.util.Memo
import shipreq.webapp.base.{URLs, WebappConfig}
import shipreq.webapp.server.data._
import shipreq.webapp.server.feature.{DiagnosticEndpoints, SessionStats}
import shipreq.webapp.server.logic._
import shipreq.webapp.server.security.Permission.RequestVarPermExt
import shipreq.webapp.server.security.{Oshiro, Permission, Permissions}

object AppSiteMap { // TODO Cleanup
  type PM[T] = Menu.ParamMenuable[T]

  // TODO SiteMap effects have a requested order. Right now they're unenforced.

  private implicit class RequestVarNExt[T](val rv: RequestVar[Name[T]]) extends AnyVal {
    def setByParam(pm: PM[T], desc: String) = setReqVar(rv, pm, desc)
  }

  private val publicTemplate = "public"

  // -------------------------------------------------------------------------------------------------------------------
  // Menu.i(NAME_AND_TITLE) / PATH_FOR_URL_AND_TEMPLATE
  // Menu(Loc(NAME, PATH_FOR_URL_AND_TEMPLATE, TITLE))
  // Menu.param[PARAM_TYPE(S)](NAME, TITLE, URL_TO_PARAM, PARAM_TO_URL) / PATH_FOR_URL_AND_TEMPLATE

  val Home =
    pageWithStaticUrl("home", defaultTitle, "Home")(_ / URLs.ForLift.memberHome
      >> UseEitherTemplate(Oshiro.isAuthenticated(), "members-home")(publicTemplate))

//  val LandingPageViaBusinessCard =
//    pageWithStaticUrl("land-bc", "")(_ / "bc" >> UseTemplate(publicTemplate))

  val Logout =
    pageWithStaticUrl("logout", defaultTitle, "Logout")(_ / URLs.ForLift.logout >> EarlyResponse(() => logout()))

//  val Register2 =
//    (Menu.param[String]("register2", "", i => Full(i), o => o) / "register" / *
//      >> StaticTitle(mkTitle("Register"))
//      >> UseTemplate("public/register2")
//      >> Hidden)

//  private def ResetPasswordTitle = mkTitle("Password Reset")
//
//  val ResetPassword2 =
//    (Menu.param[String]("resetpw2", "", i => Full(i), o => o) / "resetpw" / *
//      >> StaticTitle(ResetPasswordTitle)
//      >> UseTemplate("public/resetpw2")
//      >> Hidden)

  val Project: PM[ProjectId] =
    (MenuWithIdParam(ProjectId.Extern)("project") / URLs.ForLift.project / * / **
      >> StaticTitle(defaultTitle)
      >> AuthenticationRequired >> ProjectPermissionRequired
      >> UseTemplate("members-project")
      >> PerformEffects {
           RequestVars.ProjectId.setByParam(Project, "Project --> ProjectId")
           RequestVars.ProjectOwner.loadFromProjectId()
         })

  val AdminStats =
    pageWithStaticUrl("admin.stats", mkTitle("Stats"), "")(_ / "sir" / "stats"
      >> UseTemplate("admin-stats")
      >> AdminOnly
      >> Hidden)

  // -------------------------------------------------------------------------------------------------------------------

  private def AllProdPages: List[ConvertableToMenu] = List(
    Home,
//    LandingPageViaBusinessCard,
    Logout,
    Project,
    AdminStats
  ) ++ DiagnosticEndpoints.Endpoints

  val sitemap = {
    import org.apache.shiro.SecurityUtils.getSubject
    import org.apache.shiro.authc.UsernamePasswordToken

    def autoLogin = Menu.i("x") / "x" >> EarlyResponse(() => {
      getSubject.login(new UsernamePasswordToken("devuser", "dev123123"))
      SessionStats.onLogin(S.session, Oshiro.loggedInUser().get)
      Full(redirectHome)
      // Full(RedirectResponse("/project/oLctx/table"))
    })

    def apiLogin = Menu.i("login.api") / "login.api" >> EarlyResponse(() => for {
      u <- S.param("user")
      p <- S.param("pass")
    } yield {
      getSubject.login(new UsernamePasswordToken(u, p))
      PlainTextResponse("OK")
    })

    val additionalPages: List[ConvertableToMenu] =
      Props.mode match {
        case Development => List(autoLogin)
        case TestMode    => List(apiLogin)
        case _           => List.empty
      }

    val pages = AllProdPages ++ additionalPages

    SiteMap(pages: _*)
  }

  // -------------------------------------------------------------------------------------------------------------------

  object Implicits {
    private def BaseUrl = DI.serverConfig.baseUrl

    private def newUrlMemo(f: Loc[_] => String): Loc[_] => String =
      Memo.byRef(f)

    private val relUrlMemo = newUrlMemo { loc =>
      val s = loc.calcDefaultHref
      if (s.endsWith("/index"))
        if (s.length == 6)
          "/"
        else
          s.substring(0, s.length - 6)
      else
        s
    }

    private val absUrlMemo = newUrlMemo(
      _.relativeUrl match {
        case "/" => BaseUrl
        case s   => BaseUrl + s
      }
    )

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

  lazy val redirectHome = RedirectResponse(Home.relativeUrl)

  val LoginRelativeUrl = URLs.login
  def LoginAbsoluteUrl = DI.serverConfig.baseUrl + LoginRelativeUrl
  val redirectToLogin = RedirectResponse(LoginRelativeUrl)

  def logout(): Box[LiftResponse] = {
    Oshiro.logout()
    SessionStats.onLogout(S.session)
    Full(redirectHome)
  }

  @inline final def defaultTitle = WebappConfig.appName

  @inline final def mkTitle(title: String): String = title match {
    case "" => defaultTitle
    case _  => s"$title | ${WebappConfig.appName}"
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

//  private def TitleFromProjectName[T] =
//    DynamicTitle[T](mkTitle(RequestVars.Project.get.value.name))

  private def MenuWithIdParam[Id <: AnyRef](scheme: ExternalId.Scheme[_, Id])(name: String) = {
    val parseB: String => Box[Id] = s => Box(scheme.parseOption(s))
    Menu.param[Id](name, "", parseB, scheme.toExternal(_).value)
  }

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
    If(() => Oshiro.isAuthenticated(),
      // The # here is to clear the hash fragment from logged-in pages.
      // Means that /project/abcd#reqs/UC-1 is redirected to /login# instead of /login#reqs/UC-1
      () => redirectToLogin)

  private def PermissionRequired(checker: => Permission.Checker, failResp: LiftResponse = redirectHome) =
    If(() => checker.isPass, () => failResp)

  private def ProjectPermissionRequired =
    PermissionRequired {
      val p = RequestVars.ProjectId.get.value
      val u = RequestVars.ProjectOwner.get.value
      Permissions.accessProject.using(project = Some(ProjectId.AndOwner(p, u)))
    }

  private def AdminOnly =
    Test(_ => Permissions.admin.using().isPass)

  private def PerformEffects(f: => Unit) = Test(_ => {f; true})

  private def setReqVar[T](v: RequestVar[Name[T]], m: PM[T], desc: String): Unit =
    v.set(Need(
      m.currentValue match {
        case Full(t) => t
        case b@_ => throw new IllegalStateException(s"RequestVar get failed, menu param unavailable. ($desc: $b)")
      }
    ))
}
