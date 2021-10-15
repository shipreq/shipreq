package shipreq.webapp.server.logic.config

import cats.syntax.apply._
import japgolly.clearconfig._

final case class ScalaJsManifest[+A](public   : A,
                                     home     : A,
                                     project  : A,
                                     webWorker: A) {

  def map[B](f: A => B): ScalaJsManifest[B] =
    ScalaJsManifest(
      public    = f(public),
      home      = f(home),
      project   = f(project),
      webWorker = f(webWorker),
    )
}

object ScalaJsManifest {

  def config[A: ConfigValueParser]: ConfigDef[ScalaJsManifest[A]] =
    ( ConfigDef.need[A]("public"),
      ConfigDef.need[A]("home"),
      ConfigDef.need[A]("project"),
      ConfigDef.need[A]("webWorker"),
    ).mapN(apply)
}
