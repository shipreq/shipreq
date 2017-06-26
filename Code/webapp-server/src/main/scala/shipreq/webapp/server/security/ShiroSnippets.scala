package shipreq.webapp.server.security

import net.liftweb.http.{DispatchSnippet, LiftRules}
import org.apache.shiro.SecurityUtils.{getSubject => subject}
import scala.xml.{NodeSeq, Text}
import shipreq.webapp.server.logic.User
import Oshiro.loggedInUser

object ShiroSnippets {

  def init(): Unit = {
    LiftRules.snippetDispatch.append {
      case "Authenticated" => Authenticated
      case "AuthenticatedOrRemembered" => AuthenticatedOrRemembered
      case "LoggedInUser" => LoggedInUser
    }
  }

  /**
   * Abstract snippet that renders enclosed content if a condition is met. The `not` method can be used to inverse the
   * condition.
   */
  sealed trait ShowIfConditionSatisfied extends DispatchSnippet {
    override def dispatch = {
      case "render" => render
      case "not" => not
    }
    def cond: Boolean
    def render(view: NodeSeq): NodeSeq = if (cond) view else NodeSeq.Empty
    def not(view: NodeSeq): NodeSeq = if (cond) NodeSeq.Empty else view
  }

  object Authenticated extends ShowIfConditionSatisfied {
    override def cond = subject.isAuthenticated
  }

  object AuthenticatedOrRemembered extends ShowIfConditionSatisfied {
    override def cond = subject.isAuthenticated || subject.isRemembered
  }

  /**
   * Renders details about the currently logged-in user.
   */
  object LoggedInUser extends DispatchSnippet {
    override def dispatch = {
      case "username" => userAttribute(_.username.value)
      case "email" => userAttribute(_.email.value)
    }
    def userAttribute(fn: User => String) =
      (_: NodeSeq) => Text(loggedInUser().fold("?")(fn))
    // "* *" #> Text(loggedInUser.map(fn) getOrElse "?")
  }

}
