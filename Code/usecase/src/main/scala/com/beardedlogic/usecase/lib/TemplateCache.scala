package com.beardedlogic.usecase.lib

import scala.xml.NodeSeq
import net.liftweb.http.Templates
import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._

/**
 * Loads and caches HTML templates.
 *
 * @since 11/06/2013
 */
object TemplateCache {

  implicit class TemplateExt(val template: NodeSeq) extends AnyVal {
    def extract(id: String): NodeSeq = ExtractFromTemplate(id, template)
  }

  def LoadTemplate(path: List[String]): NodeSeq =
    ClearClearable(Templates(path) openOrThrowException s"Template not found: ${path.mkString("/")}")

  def ExtractFromTemplate(id: String, template: NodeSeq): NodeSeq = {
    var t = s"#$id ^^" #> ""
    if (id.startsWith("template-")) t = t & s"#$id [id]" #> (None: Option[String])
    val r = t(template)
    if (r.isEmpty) {
      val e = new Exception(s"Template extraction failed: $id\nTemplate length = ${template.length}")
      // e.printStackTrace()
      throw e
    }
    r
  }

  val UseCaseEditorTemplate = LoadTemplate(List("uce"))
}
