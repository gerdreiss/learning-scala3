val scala3Version = "3.1.1"

lazy val javaFxModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

lazy val root = project
  .in(file("."))
  .settings(
    name                                 := "snakefx",
    version                              := "0.1.0-SNAPSHOT",
    scalaVersion                         := scala3Version,
    libraryDependencies += "org.scalafx" %% "scalafx" % "17.0.1-R26",
    libraryDependencies ++= javaFxModules.map { m =>
      "org.openjfx" % s"javafx-$m" % "17.0.1" classifier "mac"
    }
  )

fork := true
