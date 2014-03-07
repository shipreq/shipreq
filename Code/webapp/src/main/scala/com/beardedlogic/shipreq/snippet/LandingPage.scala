package shipreq.webapp.snippet

import scalaz.{Failure, Success}
import net.liftweb.http.SHtml
import net.liftweb.http.js.{JsCmds, JsCmd}
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import shipreq.webapp.app.AppConfig
import shipreq.webapp.util.HtmlTransformExt.ajaxSubmitOnClick
import shipreq.webapp.feature.validation.Validator
import shipreq.webapp.lib.MailHelpers

object LandingPage {

  case class Interest(name: String, email: String, msg: Option[String])

  private val firstNameExtractor = "\\s.+$".r
  private val jsDisableForm = JsCmds.Run("$('#form').find('input,textarea,button').prop('disabled',true)")

  def form: CssSel = {
    var nameInput: String = ""
    var emailInput: String = ""
    var msgInput: String = ""

    def nameV = Validator.landingPageName.correctAndValidate(nameInput)
    def emailV = Validator.landingPageEmail.correctAndValidate(emailInput)
    def msgV = Validator.landingPageMsg.correctAndValidate(msgInput)

    def onSubmit: JsCmd =
      Validator.Ap.apply3(nameV, emailV, msgV)(Interest) match {
        case Failure(f) =>
          JsCmds.Alert(f.toText)
        case Success(i) =>
          processInterest(i)
          val firstName = firstNameExtractor.replaceFirstIn(i.name, "")
          jsDisableForm & JsCmds.Alert(s"Thank you, $firstName.\n\nWe'll be in touch!")
      }

    ".n" #> SHtml.onSubmit(nameInput = _) &
    ".e" #> SHtml.onSubmit(emailInput = _) &
    ".m" #> SHtml.onSubmit(msgInput = _) &
    ":submit" #> ajaxSubmitOnClick(onSubmit _)
  }

  def processInterest(i: Interest): Unit = {
    import AppConfig.LandingPage._
    import MailHelpers._
    val from = s"${i.name} <${i.email}>"
    val body = s"$from\n\n${i.msg getOrElse ""}"
    val mail = plainTextMail(subject, body) addressedTo sendTo
    sendMail(mail)
  }
}
