logLevel := Level.Warn

resolvers += Resolver.url("dancingrobot84-bintray",
  url("http://dl.bintray.com/dancingrobot84/sbt-plugins/"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.dancingrobot84" % "sbt-idea-plugin" % "0.3.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.12.0")