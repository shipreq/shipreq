package shipreq.webapp.server.logic

import japgolly.clearconfig._
import scalaz.syntax.applicative._

final case class ScalaJsManifest[+A](public   : A,
                                     home     : A,
                                     project  : A,
                                     webWorker: A)

object ScalaJsManifest {

  def config[A: ConfigValueParser]: ConfigDef[ScalaJsManifest[A]] =
    ( ConfigDef.need[A]("public") |@|
      ConfigDef.need[A]("home") |@|
      ConfigDef.need[A]("project") |@|
      ConfigDef.need[A]("webWorker")
    )(apply)
}
