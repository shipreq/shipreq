package com.beardedlogic.usecase.util

import net.liftweb.http.Templates
import net.liftweb.util.{ClearClearable, CssSel}
import net.liftweb.util.Helpers._
import xml.NodeSeq

case class NonEmptyTemplate(content: NodeSeq) {

  // This is meant to be used once on startup only. Crash if problem with templates.
  if (content.isEmpty) throw new IllegalStateException("Empty template detected.")

  def extract(css: String): NonEmptyTemplate = {
    val extractor = s"$css ^^" #> ""
    val extract = extractor(content)
    NonEmptyTemplate(extract)
  }

  def clearClearable = transform(ClearClearable)

  def removeId = transform(NonEmptyTemplate.RemoveId)

  def transform(transformer: CssSel): NonEmptyTemplate = NonEmptyTemplate(transformer(content))

  def get = content

  def quickExtract(id: String) = extract("#"+id).removeId.get
}

object NonEmptyTemplate {

 def load(path: List[String]): NonEmptyTemplate = NonEmptyTemplate(Templates(path).openOr(NodeSeq.Empty)).clearClearable

  def load(path: String): NonEmptyTemplate = load(path.split('/').toList)

  val RemoveId = "* [id]" #> (None : Option[String])
}
