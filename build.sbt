val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "adventOfCode2021",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++=
      Seq(
        "com.lihaoyi" %% "pprint" % "0.6.6"
      )
  )
