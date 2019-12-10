val dottyVersion = "0.20.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name    := "advent-of-code",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    javaOptions in run  += "-Xms256M -Xmx2G -Xss16M"
  )
