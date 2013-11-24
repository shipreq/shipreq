package com.beardedlogic.usecase.snippet

import net.liftweb.common.Box
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import scala.xml._
import com.beardedlogic.usecase.lib.NoticeFlash

object AlertType {
  private[snippet] final val CommonClasses = "alert alert-dismissable"
}

sealed trait AlertType {
  def classes: String
  def sessionMsgs: List[(NodeSeq, Box[String])]
  def flashMsgs: NoticeFlash.NoticeFlashVar
}

case object AlertTypeError extends AlertType {
  override val classes = AlertType.CommonClasses + " alert-danger"
  override def sessionMsgs = S.errors
  override def flashMsgs = NoticeFlash.errors
}

case object AlertTypeSuccess extends AlertType {
  override val classes = AlertType.CommonClasses + " alert-success"
  override def sessionMsgs = S.notices
  override def flashMsgs = NoticeFlash.notices
}

case object AlertTypeWarning extends AlertType {
  override val classes = AlertType.CommonClasses + " alert-warning"
  override def sessionMsgs = S.warnings
  override def flashMsgs = NoticeFlash.warnings
}

/**
 * Renders any notices set in S.error/warning/notice.
 */
object Notices {

  def render =
    "* *" #> (
      renderAllOfType(AlertTypeError) ++
      renderAllOfType(AlertTypeWarning) ++
      renderAllOfType(AlertTypeSuccess)
    )

  def renderAllOfType(alertType: AlertType) = {
    val s = S.noIdMessages(alertType.sessionMsgs)
    val all = alertType.flashMsgs.get match {
      case None    => s
      case Some(f) => f.list ++ s
    }
    renderMsgs(alertType, all)
  }

  /** @return Either an empty NodeSeq or an Elem */
  def renderMsgs(alertType: AlertType, msgs: Seq[NodeSeq]): NodeSeq =
    msgs match {
      case Nil        => NodeSeq.Empty
      case one :: Nil => toMsgContainer(alertType, one)
      case many       => toMsgContainer(alertType, mergeMsgs(many))
    }

  def renderSingle(alertType: AlertType, msg: NodeSeq): Elem = toMsgContainer(alertType, msg)

  def mergeMsgs(msgs: Seq[NodeSeq]): Elem = toList(msgs.map(toListItem).foldLeft(NodeSeq.Empty)(_ ++ _))

  // -------------------------------------------------------------------------------------------------------------------
  // HTML generation

  final val closeAlertButton              = <button type="button" class="close" data-dismiss="alert">&times;</button>
  private def toMsgContainer(
    at: AlertType, content: NodeSeq)      = <div class={at.classes}>{closeAlertButton}{content}</div>
  private def toList(msgs: NodeSeq)       = <ul>{msgs}</ul>
  private def toListItem(msg: NodeSeq)    = <li>{msg}</li>
}
