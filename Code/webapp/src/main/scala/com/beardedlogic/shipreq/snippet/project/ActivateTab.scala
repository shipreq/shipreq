package shipreq.webapp.snippet.project

import net.liftweb.util.Helpers._
import scalaz.Memo
import scala.xml.NodeSeq
import shipreq.webapp.util.FlashVar
import shipreq.webapp.util.HtmlTransformExt.removeClasses

object ActivateTab {

  val flash = new FlashVar[Tab]

  sealed trait Tab {
    def setInFlash(): Unit = flash.set(this)
  }
  case object UseCasesTab extends Tab
  case object SharesTab extends Tab

  def render(in: NodeSeq): NodeSeq =
    transform(flash.get)(in)

  val transform = Memo.immutableListMapMemo[Option[Tab], NodeSeq => NodeSeq] {
    case None | Some(UseCasesTab) =>
      identity
    case Some(SharesTab) =>
      "#project-body nav" #> (
        ".active [class!]" #> "active" &
        ".shares [class+]" #> "active"
      ) andThen
      "#tab-shares [class+]" #> "in active" andThen
      removeClasses("#tab-ucs")("in", "active")
  }
}
