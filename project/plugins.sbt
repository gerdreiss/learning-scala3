ThisBuild / scalaVersion := "3.0.0-M1"
ThisBuild / useSuperShell := false
ThisBuild / autoStartServer := false

update / evictionWarningOptions := EvictionWarningOptions.empty

addSbtPlugin("ch.epfl.lamp"     % "sbt-dotty"    % "0.4.2")
addSbtPlugin("com.timushev.sbt" % "sbt-updates"  % "0.5.1")
addSbtPlugin("com.typesafe.sbt" % "sbt-git"      % "1.0.0")
addSbtPlugin("io.spray"         % "sbt-revolver" % "0.9.1")
addSbtPlugin("org.scalameta"    % "sbt-scalafmt" % "2.4.2")
