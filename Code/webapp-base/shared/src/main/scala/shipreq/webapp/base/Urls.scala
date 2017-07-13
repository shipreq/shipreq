package shipreq.webapp.base

import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.util.Url
import shipreq.base.util.univeq.UnivEq
import shipreq.webapp.base.data._

object Urls {

  sealed abstract class PublicSpaRoute
  object PublicSpaRoute {

    sealed abstract class Static(val url: Url.Relative) extends PublicSpaRoute

    sealed abstract class NeedsToken(prefix: Url.Relative) extends PublicSpaRoute {
      val url = prefix.thenParam[SecurityToken](_.value)
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

  def publicHome     = PublicSpaRoute.Home.url
  def login          = PublicSpaRoute.Login.url
  def termsOfService = PublicSpaRoute.TermsOfService.url
  val memberHome     = Url.Relative("/home")
  val project        = Url.Relative("/project").thenParam[ProjectId.Public](_.value)
  val logout         = Url.Relative("/logout")
}
