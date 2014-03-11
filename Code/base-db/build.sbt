libraryDependencies ++= {
  Seq(
    "org.postgresql"            % "postgresql"             % "9.3-1101-jdbc41",
    "com.typesafe.slick"       %% "slick"                  % "1.0.1",
    "com.jolbox"                % "bonecp"                 % "0.8.0.RELEASE",
    "com.google.code.findbugs"  % "jsr305"                 % "2.0.2", // required by Guava (which is required by BoneCP)
    "com.googlecode.flyway"     % "flyway-core"            % "2.3.1",
    "ch.qos.logback"            % "logback-classic"        % "1.1.1",
    // [test]
    "org.scalatest"              %% "scalatest"              % "2.1.0"               % "test"
  )
}
