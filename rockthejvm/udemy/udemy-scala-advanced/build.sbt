val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "udemy-scala-advanced",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version
  )
