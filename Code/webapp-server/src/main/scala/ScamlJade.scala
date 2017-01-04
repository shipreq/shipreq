/**
 * Lifted Scaml mode
 *
 * Author: Tony Kay <tony.kay@gmail.com>
 */
package bootstrap.liftweb

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.net.URL
import net.liftweb.common.Box
import net.liftweb.common.Empty
import net.liftweb.common.Full
import net.liftweb.common.Loggable
import net.liftweb.http.{ContentParser, LiftRules, S}
import net.liftweb.http.provider.servlet.HTTPServletContext
import org.fusesource.scalate.layout.DefaultLayoutStrategy
import org.fusesource.scalate.scaml.ScamlOptions
import org.fusesource.scalate.util.{ClassPathBuilder, FileResourceLoader, ResourceNotFoundException}
import org.fusesource.scalate.{Binding, DefaultRenderContext, TemplateEngine}
import scala.tools.nsc.Global
import net.liftweb.util.Props
import org.apache.commons.io.IOUtils
import scala.xml.NodeSeq

/**
 * ScamlJade template support.
 *
 * This mode allows you to write files in scaml or jade which are then pre-processed into Lift templates.
 *
 * **IMPORTANT**: In development mode, the scaml/jade will be run every time;
 * however, in production mode it will only every be processed once, so it must
 * be side-effect free or it will not behave as expected.
 */
object ScamlJade extends Loggable {

  if (Props.devMode) {
    System.setProperty("scalate.allowReload", "true")
  } else {
    System.setProperty("scalate.allowReload", "false")
    ScamlOptions.indent = ""
    ScamlOptions.nl = " "
  }

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
   */
  def init(): Unit = {
    LiftRules.contentParsers ::=
      ContentParser("scaml", hamlToXml, identity[NodeSeq](_))
  }

  val hamlToXml: String => Box[NodeSeq] =
    hamlToHtml(_)
      .map(IOUtils.toInputStream(_, "UTF-8"))
      .flatMap(S.htmlProperties.htmlParser)

  def hamlToHtml(haml: String): Box[String] =
    try {
      val template = renderer.compileScaml(haml)
      Full(renderer.layout("", template))
    } catch {case e: Throwable =>
      throw scala.xml.dtd.ValidationException(e.getMessage)
    }
}

/**
 * A TemplateEngine using the Lift web abstractions.
 */
class ScamlJadeRenderer extends TemplateEngine with Loggable {
  // TODO Is all of this needed anymore?

  bindings = List(Binding("context", classOf[DefaultRenderContext].getName, true, isImplicit = true))

//  if (useWebInfWorkingDirectory) {
//    val path = realPath("WEB-INF")
//    if (path ne null) {
//      workingDirectory = new File(path, "_scalate")
//      workingDirectory.mkdirs
//    }
//  }
  classpath = buildClassPath()
  resourceLoader = new LiftResourceLoader(this)
  layoutStrategy = new DefaultLayoutStrategy(this, "/WEB-INF/scalate/layouts/default.scaml", "/WEB-INF/scalate/layouts/default.ssp")

  private def buildClassPath(): String = {
    val builder = new ClassPathBuilder

    // Add containers class path
    builder.addPathFrom(getClass)
    .addPathFrom(classOf[TemplateEngine])
    .addPathFrom(classOf[Product])
    .addPathFrom(classOf[Global])

    // Always include WEB-INF/classes and all the JARs in WEB-INF/lib just in case
    builder.addClassesDir(realPath("/WEB-INF/classes"))
    .addLibDir(realPath("/WEB-INF/lib"))

    builder.classPath
  }

  def useWebInfWorkingDirectory = {
    val customWorkDir = System.getProperty("scalate.workingdir", "")
    val property = System.getProperty("scalate.temp.workingdir", "")
    property.toLowerCase != "true" && customWorkDir.length <= 0
  }

  def realPath(uri: String): String = {
    LiftRules.context match {
      case http: HTTPServletContext => http.ctx.getRealPath(uri)
      case c => uri //logger.warn("Do not know how to get the real path of: " + uri + " for context: " + c); uri
    }
  }

  class LiftResourceLoader(context: ScamlJadeRenderer) extends FileResourceLoader {
    override protected def toFile(uri: String) = {
      realFile(uri)
    }

    protected def toFileOrFail(uri: String): File = {
      val file = realFile(uri)
      if (file eq null) {
        throw new ResourceNotFoundException(resource = uri, root = context.realPath("/"))
      }
      file
    }

    /**
     * Returns the real path for the given uri
     */
    def realPath(uri: String): String = {
      val file = realFile(uri)
      if (file ne null) file.getPath else null
    }

    /**
     * Returns the File for the given uri
     */
    def realFile(uri: String): File = {
      def findFile(uri: String): File = {
        val path = context.realPath(uri)
        //logger.debug("realPath for: " + uri + " is: " + path)

        var answer: File = null
        if (path ne null) {
          val file = new File(path)
          if (file.canRead) { answer = file }
        }
        answer
      }

      findFile(uri) match {
        case file: File => file
        case _ => if (uri.startsWith("/") && !uri.startsWith("/WEB-INF")) {
          findFile("/WEB-INF" + uri)
        } else {
          null
        }
      }
    }
  }
}
