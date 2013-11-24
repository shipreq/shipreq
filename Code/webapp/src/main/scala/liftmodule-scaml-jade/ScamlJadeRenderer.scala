/*
 * Copyright 2010-2013 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Author: The original author of the liftmodule-scalate.
 */

package net.liftmodules.scamljade
import java.io.File
import tools.nsc.Global
import org.fusesource.scalate.layout.DefaultLayoutStrategy
import org.fusesource.scalate.util.{ ClassPathBuilder, FileResourceLoader }
import org.fusesource.scalate.{ DefaultRenderContext, ResourceNotFoundException, Binding, TemplateEngine }
import net.liftweb._
import common._
import http.LiftRules
import http.provider.servlet.HTTPServletContext

/**
 * A TemplateEngine using the Lift web abstractions.
 */
class ScamlJadeRenderer extends TemplateEngine with Loggable {
  bindings = List(Binding("context", classOf[DefaultRenderContext].getName, true, isImplicit = true))

  if (useWebInfWorkingDirectory) {
    val path = realPath("WEB-INF")
    if (path != null) {
      workingDirectory = new File(path, "_scalate")
      workingDirectory.mkdirs
    }
  }
  classpath = buildClassPath
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
      if (file == null) {
        throw new ResourceNotFoundException(resource = uri, root = context.realPath("/"))
      }
      file
    }

    /**
     * Returns the real path for the given uri
     */
    def realPath(uri: String): String = {
      val file = realFile(uri)
      if (file != null) file.getPath else null
    }

    /**
     * Returns the File for the given uri
     */
    def realFile(uri: String): File = {
      def findFile(uri: String): File = {
        val path = context.realPath(uri)
        logger.debug("realPath for: " + uri + " is: " + path)

        var answer: File = null
        if (path != null) {
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
