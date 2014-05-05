package shipreq.webapp.test

import net.liftweb.util.Helpers._
import org.scalatest.Assertions
import xml.{Node, NodeSeq}

trait CssTestHelpers {
  this: Assertions =>

  //  def getNodeClasses(n: Node): List[String] = n.attribute("class").map(_.toString.split("\\s+").toList).getOrElse(Nil)
  //  def hasClass(className: String)(n: Node) = getNodeClasses(n).contains(className)
  //  def findNodeO(xml: NodeSeq, tag: String, className: String) = xml \\ tag filter hasClass(className) headOption
  //  def findNode(xml: NodeSeq, tag: String, className: String) = findNodeO(xml, tag, className) match {
  //    case None => fail(s"Node not found: $tag.$className")
  //    case Some(n) => n
  //  }

  def findCssO(xml: NodeSeq, limitedLiftCss: String): Option[Node] = {
    val sel = s"$limitedLiftCss ^^" #> ""
    val r = sel(xml)
    if (r == xml) None
    else r.headOption
  }

  def findCss(xml: NodeSeq, limitedLiftCss: String) = findCssO(xml, limitedLiftCss) match {
    case None => fail(s"Node not found: $limitedLiftCss")
    case Some(n) => n
  }

}
