package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import shipreq.base.util.FxModule._
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.lib.SnippetHelpers
import shipreq.webapp.server.protocol._

object PublicSpa extends SnippetHelpers {

  val EntryPoint = ClientSideProcInvoker(PublicSpaProtocols.EntryPoint)

  def render = {
//    val initData = Global.logic.publicSpa.initData.unsafeRun()
    import scalaz.std.option.optionInstance
    import scalaz.syntax.traverse._

    val initFx =
      for {
        initData <- Global.logic.publicSpa.initData
        html <- Global.ssr.traverse(_.public(initData))
      } yield {
        html.foreach { x =>
          println()
          println(x)
          println()
        }
        initData
      }

    val initData = initFx.unsafeRun()

    "*" #> EntryPoint.invokeOnLoadHtml(initData)
  }
}
