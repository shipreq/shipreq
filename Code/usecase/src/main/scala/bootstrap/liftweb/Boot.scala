package bootstrap.liftweb

import net.liftweb.http._
import net.liftweb.sitemap.{ Menu, SiteMap }
import com.beardedlogic.usecase._
import lib.db.DB
import lib.{ExternalIdStr, Misc, Defaults}
import api.UseCaseApi
import com.beardedlogic.usecase.snippet.UCEditor

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
    Misc.ensureTestModeDuringTests()

    // App package path
    LiftRules.addToPackages("com.beardedlogic.usecase")

    // Build SiteMap
    def sitemap(): SiteMap = SiteMap(
      Menu.i("Home") / "index"
    )

    // Register APIs
    LiftRules.statelessDispatch.append(UseCaseApi)

    // Routing
    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath("usecase" :: ExternalIdStr(id) :: Nil, "", true, false), GetRequest, _) =>
        RewriteResponse("uce" :: Nil, Map(UCEditor.ParamId -> id))
    }

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))

    // Force requests to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))
  }

  def initDatabase() {
    DB.init()
    Defaults.init()
  }
}
