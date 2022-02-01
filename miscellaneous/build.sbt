val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "miscellaneous",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "eu.timepit"             %% "refined"                  % "0.9.28",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
      ("io.estatico"           %% "newtype"                  % "0.4.4").cross(CrossVersion.for3Use2_13)
    )
  )
