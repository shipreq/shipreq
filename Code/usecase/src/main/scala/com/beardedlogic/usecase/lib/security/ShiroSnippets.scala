package com.beardedlogic.usecase.lib.security

import net.liftweb.http.{LiftRules, DispatchSnippet}
import org.apache.shiro.SecurityUtils.{getSubject => subject}
import scala.xml.{Text, NodeSeq}
import Oshiro.loggedInUser
import com.beardedlogic.usecase.model.UserDescriptor

object ShiroSnippets {

  def init() {
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
      case "username" => userAttribute(_.username)
      case "email" => userAttribute(_.email)
    }
    def userAttribute(fn: UserDescriptor => String) =
      (_: NodeSeq) => Text(loggedInUser.map(fn) getOrElse "?")
    // "* *" #> Text(loggedInUser.map(fn) getOrElse "?")
  }

}
