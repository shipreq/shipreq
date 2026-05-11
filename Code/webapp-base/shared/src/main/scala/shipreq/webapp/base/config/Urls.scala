package shipreq.webapp.base.config

import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.util.Url
import shipreq.webapp.base.data._
import shipreq.webapp.base.util.Obfuscated

object Urls {

  sealed abstract class PublicSpaRoute
  object PublicSpaRoute {

    sealed abstract class Static(val url: Url.Relative) extends PublicSpaRoute

    sealed abstract class NeedsToken(val prefix: Url.Relative) extends PublicSpaRoute {
      val url = prefix.thenParam[VerificationToken](_.value)
    }

    case object Home           extends Static(Url.Relative("/"))
    case object Login          extends Static(Url.Relative("/login"))
    case object Privacy        extends Static(Url.Relative("/privacy"))
    case object Register1      extends Static(Url.Relative("/register"))
    case object TermsOfService extends Static(Url.Relative("/terms"))

    case object Register2      extends NeedsToken(Register1.url)
    case object ResetPassword  extends NeedsToken(Url.Relative("/reset/password"))

    implicit def univEqStatic    : UnivEq[Static        ] = UnivEq.derive
    implicit def univEqNeedsToken: UnivEq[NeedsToken    ] = UnivEq.derive
    implicit def univEqRoute     : UnivEq[PublicSpaRoute] = UnivEq.derive

    val static     = AdtMacros.adtValues[Static]
    val needsToken = AdtMacros.adtValues[NeedsToken]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  sealed abstract class MemberRoute[-A]
  object MemberRoute {

    sealed abstract class Static(val url: Url.Relative) extends MemberRoute[Any]

    sealed abstract class Param1[-A](val url: Url.Relative.Param1[A]) extends MemberRoute[A] {
      @inline final def prefix = url.prefix
    }

    case object Home                 extends Static(Url.Relative("/home"))
    case object Logout               extends Static(Url.Relative("/logout"))
    case object Project              extends Param1(Url.Relative("/project").thenParam[ProjectId.Public](_.value))
    case object ProjectAccessRevoked extends Static(Url.Relative("/project-revoked"))

    implicit def univEqStatic: UnivEq[Static] = UnivEq.derive
    val static = AdtMacros.adtValues[Static]
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object ProjectSpaWebSocket {
    final val ParamProjectId = "p"
    final val ParamCreator   = "c"
    final val Base           = "/w/p"
    final val ServerEndpoint = Base + "/{" + ParamProjectId + "}" + "/{" + ParamCreator + "}"

    type Param1 = ProjectId.Public
    type Param2 = ProjectCreator

    val url = Url.Relative(ProjectSpaWebSocket.Base)
                .thenParam((_: Param1).value)
                .thenParam((_: Param2).userId.value)

    def parsePath(path: String): Option[(Param1, Param2)] = {
      val tail  = path.substring(Base.length + 1)
      val colon = tail.indexOf('/')
      Option.when(colon > 0) {
        val p1: Param1 = Obfuscated(tail.take(colon))
        val p2: Param2 = ProjectCreator(Obfuscated(tail.drop(colon + 1)))
        (p1, p2)
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final case class External private[External](title: String, url: Url.Absolute)

  object External {
    def about = apply("About", Url.Absolute("https://blog.shipreq.com/about"))
    def blog  = apply("Blog", Url.Absolute("https://blog.shipreq.com"))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val ajaxRoot = Url.Relative("/x")

  def publicHome           = PublicSpaRoute.Home.url
  def login                = PublicSpaRoute.Login.url
  def termsOfService       = PublicSpaRoute.TermsOfService.url
  def memberHome           = MemberRoute.Home.url
  def project              = MemberRoute.Project.url
  def projectAccessRevoked = MemberRoute.ProjectAccessRevoked.url
  def logout               = MemberRoute.Logout.url
}
