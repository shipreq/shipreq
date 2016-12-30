package shipreq.webapp.server.snippet

import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import scalaz.{Failure, Success}
import shipreq.taskman.api.Msg.LandingPageHit
import shipreq.webapp.server.feature.validation.Validators
import shipreq.webapp.server.lib.{FormVar, SnippetHelpers}
import shipreq.webapp.server.util.HtmlTransformExt.ajaxSubmitOnClick

object LandingPage extends SnippetHelpers {

  private val firstNameExtractor = "\\s.+$".r
  private val jsDisableForm = JsCmds.Run("$('#form').find('input,textarea,button').prop('disabled',true)")

  private val formDef = FormVar.merge(
    FormVar.strOnSubmit(Validators.landingPage.email, ".e"),
    FormVar.strOnSubmit(Validators.landingPage.name, ".n"),
    FormVar.strOnSubmit(Validators.landingPage.msg, ".m"),
    FormVar.boolOnSubmit(".newsletter input")
  )(LandingPageHit)

  def form: CssSel = {
    var vars: formDef.Var = ("", "", "", true)

    def onSubmit: JsCmd =
      formDef.validate(vars) match {
        case Failure(f) =>
          JsCmds.Alert(f.toText)
        case Success(msg) =>
          taskman().submitMsg(msg).unsafePerformIO()
          val firstName = firstNameExtractor.replaceFirstIn(msg.name, "")
          jsDisableForm & JsCmds.Alert(s"Thank you, $firstName.\n\nWe'll be in touch!")
      }

    formDef.csssel(vars, vars = _) & ":submit" #> ajaxSubmitOnClick(onSubmit _)
  }
}
