package shipreq.webapp.base

import japgolly.microlibs.adt_macros.AdtMacros
import shipreq.base.util.Url
import shipreq.webapp.base.data.SecurityToken

object PublicUrls {
  sealed abstract class PublicSpaRoute
  object PublicSpaRoute {
    sealed abstract class Static(val url: Url.Relative) extends PublicSpaRoute
    sealed abstract class NeedsToken(prefix: Url.Relative) extends PublicSpaRoute {
      val url = prefix.thenParam[SecurityToken](_.value)
    }
    case object Home           extends Static(Url.Relative("/"))
    case object Login          extends Static(Url.Relative("/login"))
    case object Register1      extends Static(Url.Relative("/register"))
    case object Register2      extends NeedsToken(Register1.url)
    case object ResetPassword1 extends Static(Url.Relative("/resetpw"))
    case object ResetPassword2 extends NeedsToken(ResetPassword1.url)

    val static     = AdtMacros.adtValues[Static]
    val needsToken = AdtMacros.adtValues[NeedsToken]
  }

  def home           = PublicSpaRoute.Home          .url
  def login          = PublicSpaRoute.Login         .url
  def register1      = PublicSpaRoute.Register1     .url
  def register2      = PublicSpaRoute.Register2     .url
  def resetPassword1 = PublicSpaRoute.ResetPassword1.url
  def resetPassword2 = PublicSpaRoute.ResetPassword2.url
}
