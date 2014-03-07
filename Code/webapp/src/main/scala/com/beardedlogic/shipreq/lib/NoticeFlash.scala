package shipreq.webapp.lib

import scala.xml.{Text, NodeSeq}
import scalaz.NonEmptyList
import shipreq.webapp.util.ListFlashVar

object NoticeFlash {

  sealed class NoticeFlashVar extends ListFlashVar[NodeSeq] {
    def addS(msg: String) = add1(Text(msg))
    def addS(msgs: NonEmptyList[String]) = add(msgs.map(Text.apply))
  }

  val errors = new NoticeFlashVar
  val warnings = new NoticeFlashVar
  val notices = new NoticeFlashVar
}
