package com.beardedlogic.usecase.snippet

import net.liftweb.http.S
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import scala._
import net.liftweb.common.Box

/**
 * Renders any notices set in S.error/warning/notice.
 */
object Notices {

  def render =
    "*" #> containContainers(
      renderMsgs("notice_e", "alert alert-error", S.errors) ++
      renderMsgs("notice_w", "alert", S.warnings) ++
      renderMsgs("notice_n", "alert alert-success ", S.notices)
    )

  def renderMsgs(containerId: String, classes: String, msgs: List[(NodeSeq, Box[String])]): NodeSeq =
    S.noIdMessages(msgs) match {
      case Nil        => NodeSeq.Empty
      case one :: Nil => toMsgContainer(containerId, classes, one)
      case many       => toMsgContainer(containerId, classes + " alert-block", mergeMsgs(many))
    }

  def mergeMsgs(msgs: List[NodeSeq]): NodeSeq = toList(msgs.map(toListItem).foldLeft(NodeSeq.Empty)(_ ++ _))

  def containContainers(containers: NodeSeq): NodeSeq =
    if (containers == NodeSeq.Empty) NodeSeq.Empty
    else toTopContainer(containers)

  // -------------------------------------------------------------------------------------------------------------------
  // HTML generation

  final val closeAlertButton              = <button type="button" class="close" data-dismiss="alert">&times;</button>
  def toTopContainer(containers: NodeSeq) = <div id="notices">{containers}</div>
  def toMsgContainer(id: String,
    classes: String, content: NodeSeq)    = <div id={id} class={classes}>{closeAlertButton}{content}</div>
  def toList(msgs: NodeSeq)               = <ul>{msgs}</ul>
  def toListItem(msg: NodeSeq)            = <li>{msg}</li>
}
