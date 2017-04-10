package shipreq.webapp.server.snippet

import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import scalaz.{-\/, \/-}
import shipreq.taskman.api.Msg.LandingPageHit
import shipreq.webapp.base.vali2.Composite.Invalidity
import shipreq.webapp.server.feature.validation.ServerSideValidators
import shipreq.webapp.server.lib.{FormVar, SnippetHelpers}
import shipreq.webapp.server.util.HtmlTransformExt.ajaxSubmitOnClick

object LandingPage extends SnippetHelpers {

  private val firstNameExtractor = "\\s.+$".r
  private val jsDisableForm = JsCmds.Run("$('#form').find('input,textarea,button').prop('disabled',true)")

  private val formDef = FormVar.merge(
    FormVar.strOnSubmit(ServerSideValidators.landingPage.email.named, ".e"),
    FormVar.strOnSubmit(ServerSideValidators.landingPage.name.named, ".n"),
    FormVar.strOnSubmit(ServerSideValidators.landingPage.msg.named, ".m"),
    FormVar.boolOnSubmit(".newsletter input")
  )(LandingPageHit)

  def form: CssSel = {
    var vars: formDef.Var = ("", "", "", true)

    def onSubmit: JsCmd =
      formDef.validate(vars) match {
        case -\/(f) =>
          JsCmds.Alert(Invalidity.toText(f))
        case \/-(msg) =>
          taskman().submitMsg(msg).unsafePerformIO()
          val firstName = firstNameExtractor.replaceFirstIn(msg.name, "")
          jsDisableForm & JsCmds.Alert(s"Thank you, $firstName.\n\nWe'll be in touch!")
      }

    formDef.csssel(vars, vars = _) & ":submit" #> ajaxSubmitOnClick(onSubmit _)
  }
}
