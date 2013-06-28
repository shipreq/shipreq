package bootstrap.liftweb

import net.liftweb.http._
import net.liftmodules.scamljade.ScamlJade
import com.beardedlogic.usecase._
import lib.{ExternalIdStr, Defaults}
import lib.db.DB
import lib.security.Oshiro
import api.UseCaseApi
import snippet.UCEditor

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    configureLift
    initDatabase
  }

  def configureLift() {

    Oshiro.init()

    // App package path
    LiftRules.addToPackages("com.beardedlogic.usecase")

    // Register APIs
    LiftRules.statelessDispatch.append(UseCaseApi)

    // Routing
    // TODO should be done via sitemap
    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath("usecase" :: ExternalIdStr(id) :: Nil, "", true, false), GetRequest, _) =>
        RewriteResponse("uce" :: Nil, Map(UCEditor.ParamId -> id))
    }

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))

    // Force requests to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Add support for HAML/Jade template (must be after other LiftRules)
    ScamlJade.init(List("scaml", "html"))
  }

  def initDatabase() {
    DB.init()
    Defaults.init()
  }
}
