val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "miscellaneous",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-explain",
        "-feature",
        "-language:implicitConversions",
        "-unchecked",
        "-Xfatal-warnings",
        "-Yexplicit-nulls", // experimental (I've seen it cause issues with circe)
        "-Ykind-projector",
        "-Ysafe-init"       // experimental (I've seen it cause issues with circe)
      ) ++ Seq("-rewrite", "-indent") ++ Seq("-source", "future-migration"),
    libraryDependencies ++= Seq(
      "eu.timepit"             %% "refined"                  % "0.9.28",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
      ("io.estatico"           %% "newtype"                  % "0.4.4").cross(CrossVersion.for3Use2_13),
      "org.typelevel"          %% "cats-core"                % "2.7.0",
      "dev.zio"                %% "zio"                      % "2.0.0-RC3",
      "dev.zio"                %% "zio-prelude"              % "1.0.0-RC11-2"
    )
  )
