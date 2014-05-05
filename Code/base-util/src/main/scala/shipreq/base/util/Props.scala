package shipreq.base.util

import java.io.InputStream
import java.util.Properties
import scalaz.Endo
import shipreq.base.util.log.HasLogger

object Props extends HasLogger {

  def loadFromClasspath(filename: String) = Endo[Properties](p => {
    val f = filename.replaceFirst("^/*", "/")
    val i = getClass.getResourceAsStream(f)
    if (i eq null)
      log.debug.z(s"Properties not found: $f")
    else {
      log.info.z(s"Loading properties: $f")
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
  def loadUsingStandardStrategy(rm: RunMode): Endo[Properties] = {
    val s = systemProps andThen loadFromClasspath("default.props")
    val f = RunMode.filenames(rm)(_.mkString("", ".", ".props"))
    (s /: f.map(loadFromClasspath))(_ andThen _)
  }
}
