val scala3Version   = "3.1.0"
val doobieVersion   = "1.0.0-RC2"
val monocleVersion  = "3.1.0"
val typenameVersion = "1.0.0"
val newTypeVersion  = "0.4.4"
val http4sVersion   = "1.0.0-M31"
val circeVersion    = "0.15.0-M1"
val fs2Version      = "3.2.4"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "rockthejvm-youtube",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core"         % doobieVersion,
      "org.tpolecat" %% "doobie-postgres"     % doobieVersion,
      "org.tpolecat" %% "doobie-hikari"       % doobieVersion,
      "dev.optics"   %% "monocle-core"        % monocleVersion,
      "dev.optics"   %% "monocle-macro"       % monocleVersion,
      "org.tpolecat" %% "typename"            % typenameVersion,
      ("io.estatico" %% "newtype"             % newTypeVersion).cross(CrossVersion.for3Use2_13),
      "org.http4s"   %% "http4s-blaze-server" % http4sVersion,
      "org.http4s"   %% "http4s-circe"        % http4sVersion,
      "org.http4s"   %% "http4s-dsl"          % http4sVersion,
      "io.circe"     %% "circe-generic"       % circeVersion,
      "co.fs2"       %% "fs2-core"            % fs2Version
    )
  )
