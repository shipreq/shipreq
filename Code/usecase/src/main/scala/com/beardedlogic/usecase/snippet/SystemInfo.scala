package com.beardedlogic.usecase.snippet

import java.util.ResourceBundle
import net.liftweb.http.S
import net.liftweb.util.Helpers.strToCssBindPromoter
import org.joda.time.DateTime
import scala.util.Properties

object SystemInfo {

  object Build {
    private val props = ResourceBundle.getBundle("build")
    private def get(key: String) = props.getString("build." + key)
    val VersionBase = get("version.base")
    val VersionFull = get("version.full")
    val Revision    = get("revision")
    val TimeStr     = get("time")
    val Time        = new DateTime(new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").parse(TimeStr))
  }

  def render = "*" #> value(S.attr("name").openOrThrowException("Attribute 'name' required."))

  def value(name: String): String = name match {
    case "build.version.base" => Build.VersionBase
    case "build.version.full" => Build.VersionFull
    case "build.revision"     => Build.Revision
    case "build.time"         => Build.TimeStr
    case "java.version"       => Properties.javaVersion
    case "jvm.version"        => Properties.javaVmVersion
    case "scala.version"      => Properties.versionNumberString
  }

  // TODO uptime, memory/gc, load?, users online, row counts
}
