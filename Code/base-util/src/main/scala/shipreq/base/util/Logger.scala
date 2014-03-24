package shipreq.base.util

import org.slf4j.LoggerFactory

object Logger {

  def forClass(c: Class[_]) = LoggerFactory.getLogger(loggingName(c))

  def loggingName(c: Class[_]): String =
    if (c.getCanonicalName ne null)
      c.getCanonicalName.replaceFirst("\\$$", "")
    else
      c.toString.replaceFirst("^Class ", "")

}

trait Logger {

  final val log = Logger.forClass(getClass)

}