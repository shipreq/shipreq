package shipreq.webapp.snippet

import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import scalaz.{Failure, Success}
import shipreq.taskman.api.Msg.LandingPageHit
import shipreq.webapp.feature.validation.Validators
import shipreq.webapp.lib.{FormVar, SnippetHelpers}
import shipreq.webapp.util.HtmlTransformExt.ajaxSubmitOnClick

object LandingPage extends SnippetHelpers {

  private val firstNameExtractor = "\\s.+$".r
  private val jsDisableForm = JsCmds.Run("$('#form').find('input,textarea,button').prop('disabled',true)")

  def form: CssSel = {
    val vars = FormVar.AP4(
      FormVar.strOnSubmit(Validators.landingPage.email, ".e")(""),
      FormVar.strOnSubmit(Validators.landingPage.name, ".n")(""),
      FormVar.strOnSubmit(Validators.landingPage.msg, ".m")(""),
      FormVar.boolOnSubmit(".newsletter input")(true)
    )

    def onSubmit: JsCmd =
      vars.validate(LandingPageHit) match {
        case Failure(f) =>
          JsCmds.Alert(f.toText)
        case Success(msg) =>
          taskman1(_ submitMsg msg)
          val firstName = firstNameExtractor.replaceFirstIn(msg.name, "")
          jsDisableForm & JsCmds.Alert(s"Thank you, $firstName.\n\nWe'll be in touch!")
      }

    vars.csssel & ":submit" #> ajaxSubmitOnClick(onSubmit _)
  }
}
