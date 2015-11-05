addSbtPlugin("org.scala-lang.modules.scalajs" % "scalajs-sbt-plugin" % "0.5.5")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

resolvers += "spray repo" at "http://repo.spray.io"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

addSbtPlugin("com.lihaoyi" % "workbench" % "0.2.1")

addSbtPlugin("com.lihaoyi" % "utest-js-plugin" % "0.2.3")
