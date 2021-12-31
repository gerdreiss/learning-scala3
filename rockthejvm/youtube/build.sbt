val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "rockthejvm",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "dev.optics"   %% "monocle-core"  % "3.1.0",
      "dev.optics"   %% "monocle-macro" % "3.1.0",
      "org.tpolecat" %% "typename"      % "1.0.0",
      ("io.estatico" %% "newtype"       % "0.4.4").cross(CrossVersion.for3Use2_13)
    )
  )
