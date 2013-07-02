package com.beardedlogic.usecase.snippet

import net.liftweb.http.{S, DispatchSnippet}
import net.liftweb.sitemap.SiteMap
import scala.xml.{Group, NodeSeq}
import com.beardedlogic.usecase.lib.SnippetHelpers

/**
 * Creates a link to a page. Throws an error is the page is not found.
 */
object Link extends DispatchSnippet with SnippetHelpers {
  override def dispatch = { case "to" => to }

  def to(text: NodeSeq): NodeSeq =
    for {
      name <- S.attr("name").toList
      loc <- SiteMap.findLoc(name) ~> cantGenerateLink_!(name)
    } yield
      Group(SiteMap.buildLink(name))

  private def cantGenerateLink_!(name: String): Nothing = shouldNeverHappen_!(s"Unable to generate link to $name")
}
