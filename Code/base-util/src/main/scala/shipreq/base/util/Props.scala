package shipreq.base.util

import java.io.InputStream
import java.util.{Locale, Properties}
import scalaz.Endo
import scalaz.std.list.listInstance
import scalaz.syntax.applicative._

object Props {

  protected val log = Logger.forClass(getClass)

  def loadFromClasspath(filename: String) = Endo[Properties](p => {
    val f = filename.replaceFirst("^/*", "/")
    val i = getClass.getResourceAsStream(f)
    if (i eq null)
      log.debug("Properties not found: {}", f)
    else {
      log.info("Loading properties: {}", f)
      p.load(i)
    }
    p
  })

  def load(i: InputStream) = Endo[Properties](p => {
    if (i != null) p.load(i)
    p
  })

  def systemProps = Endo[Properties](p => {
    p.putAll(System.getProperties)
    p
  })

  /**
   * Standard strategy of acquiring properties. In order of priority:
   *
   * [<run-mode>.][<user>.]props
   * default.props
   * System props
   */
  def loadUsingStandardStrategy(m: RunMode): Endo[Properties] = {
    def mkFilename(components: String*): Option[String] = {
      val cs = components.filter(c => (c ne null) && c.nonEmpty)
      if (cs.isEmpty) None else Some(cs.mkString("", ".", ".props"))
    }

    val runModeNames = m.names.map(_.toLowerCase(Locale.ENGLISH))
    val userNames = List(System.getProperty("user.name"), "")
    val filenames = (runModeNames |@| userNames)((a,b) => mkFilename(a,b)).filter(_.nonEmpty).map(_.get)

    val s = systemProps andThen loadFromClasspath("default.props")
    (s /: filenames.distinct.map(loadFromClasspath))(_ andThen _)
  }
}
