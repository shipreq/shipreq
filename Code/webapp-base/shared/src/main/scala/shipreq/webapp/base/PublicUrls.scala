package shipreq.webapp.base

import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.util.Url
import shipreq.base.util.univeq.UnivEq
import shipreq.webapp.base.data.SecurityToken

object PublicUrls {

  sealed abstract class SpaRoute

  object SpaRoute {

    sealed abstract class Static(val url: Url.Relative) extends SpaRoute

    sealed abstract class NeedsToken(prefix: Url.Relative) extends SpaRoute {
      val url = prefix.thenParam[SecurityToken](_.value)
    }

    case object Home           extends Static(Url.Relative("/"))
    case object Login          extends Static(Url.Relative("/login"))
    case object Privacy        extends Static(Url.Relative("/privacy"))
    case object Register1      extends Static(Url.Relative("/register"))
    case object TermsOfService extends Static(Url.Relative("/tos"))

    case object Register2      extends NeedsToken(Register1.url)
    case object ResetPassword  extends NeedsToken(Url.Relative("/reset/password"))

    implicit def univEqStatic    : UnivEq[Static    ] = UnivEq.derive
    implicit def univEqNeedsToken: UnivEq[NeedsToken] = UnivEq.derive
    implicit def univEqRoute     : UnivEq[SpaRoute  ] = UnivEq.derive

    val static     = AdtMacros.adtValues[Static]
    val needsToken = AdtMacros.adtValues[NeedsToken]
  }

  def home = SpaRoute.Home.url
  def login = SpaRoute.Login.url
}
