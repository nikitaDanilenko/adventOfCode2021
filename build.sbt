val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "adventOfCode2021",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++=
      Seq(
        "com.lihaoyi"   %% "pprint"         % "0.6.6",
        "org.typelevel" %% "spire"          % "0.18.0-M2",
        "dev.optics"    %% "monocle-unsafe" % "3.1.0",
        "org.typelevel" %% "cats-parse"     % "0.3.6"
      )
  )
