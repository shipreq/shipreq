/**
 * Lifted Scaml mode
 *
 * Author: Tony Kay <tony.kay@gmail.com>
 */
package net.liftmodules.scamljade

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.net.URL
import net.liftweb.common.Box
import net.liftweb.common.Empty
import net.liftweb.common.Full
import net.liftweb.common.Loggable
import net.liftweb.http.LiftRules

/**
 * ScamlJade template support.
 *
 * This mode allows you to write files in scaml or jade which are then pre-processed into Lift templates.
 *
 * **IMPORTANT**: In development mode, the scaml/jade will be run every time;
 * however, in production mode it will only every be processed once, so it must
 * be side-effect free or it
 * will not behave as expected.
 */
object ScamlJade extends Loggable {
  private val renderer = new ScamlJadeRenderer

  /**
   * Initialize the module. IMPORTANT: THIS CHANGES LiftRules, and must be done at the END of your boot.
   *
   * This installs Scalate as a preprocessor for templates in LiftRules getResource, and adds scaml and jade
   * as supported template suffixes.
   *
   * This initialization routine replaces the Lift resource loader, and rewrites the allowed template suffixes.
   *
   * The list of suffixes should be in the order or most commonly used first, as
   * Lift scans all of them in order until it finds one.
   *
   * @param suffixesToSupport The list of ALL suffixes your webapp will use for templates (e.g. html, scaml, jade).
   */
  def init(suffixesToSupport : List[String]) = {
    // Add support for the suffixes
    LiftRules.templateSuffixes = suffixesToSupport

    // Plug into the resource read pipeline
    val defaultGetResource = LiftRules.getResource
    def preprocessor(name: String): Box[java.net.URL] = {
      if (!name.contains("/_resources") && (name.endsWith(".jade") || name.endsWith(".scaml"))) preprocess(name)
      else defaultGetResource(name)
    }
    LiftRules.getResource = preprocessor
  }

  def preprocess(name: String): Box[URL] = {
    var fos: FileOutputStream = null
    try {
      if (renderer.canLoad(name)) {
        val rawTemplate = renderer.layout(name)
        val file: File = File.createTempFile("lift_scaml_jade", "preprocess")
        fos = new FileOutputStream(file)
        val writer = new PrintWriter(new OutputStreamWriter(fos))
        writer.print(rawTemplate)
        writer.close()
        Full(file.toURI.toURL)
      } else {
        Empty
      }
    } catch {
      case e: Throwable =>
        throw scala.xml.dtd.ValidationException(e.getMessage)
    } finally {
      if (fos != null) fos.close
      Empty
    }
  }
}
