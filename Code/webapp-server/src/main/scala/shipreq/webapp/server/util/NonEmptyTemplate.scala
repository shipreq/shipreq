package shipreq.webapp.server.util

import net.liftweb.http.Templates
import net.liftweb.util.Helpers._
import net.liftweb.util.{ClearClearable, CssSel}
import scala.xml.{Node, NodeSeq}

case class NonEmptyTemplate(content: NodeSeq) {

  // This is meant to be used once on startup only. Crash if problem with templates.
  if (content.isEmpty) throw new IllegalStateException("Empty template detected.")
  if (content.toString.contains("snippeterror")) throw new IllegalStateException(s"Snippet Error detected:\n\n$content")

  def extract(css: String): NonEmptyTemplate = {
    val extractor = s"$css ^^" #> ""
    val extracted = extractor(content)
    NonEmptyTemplate(extracted)
  }

  def clearClearable = transform(ClearClearable)

  def removeId = transform(NonEmptyTemplate.RemoveId)

  def transform(transformer: CssSel): NonEmptyTemplate = NonEmptyTemplate(transformer(content))

  def get = content

  def quickExtractById(id: String) = extract("#"+id).removeId.get

  def assert(f: NodeSeq => Boolean) =
    if (f(content)) this else
      throw new IllegalStateException(s"Template assertion failed.\n\n$content")

  def assertHead(f: Node => Boolean) = assert(c => f(c.head))

  /**
   * Checked the XML element type of the head node.
   * @param expected Eg. "form", "div".
   */
  def assertHeadType(expected: String) = assertHead(_.label == expected)

  def assertSingleHead() = assert(_.size == 1)
}

object NonEmptyTemplate {

  def load(path: List[String]): NonEmptyTemplate =
    NonEmptyTemplate(Templates(path).openOr(NodeSeq.Empty)).clearClearable

  def load(path: String): NonEmptyTemplate =
    load(path.split('/').toList)

  val RemoveId = "* [id]" #> (None : Option[String])
}
