// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Secondary project build.sbt file

val jvmSettings = JvmSettings("1.8", "1.8", "1.8")

// JVM settings can be verified using the following command:
// javap -verbose -cp secondary.jar bdzimmer.secondary.export.Driver
// Major version will be 52 for Java 1.8.

lazy val root = (project in file("."))
  .settings(
    name         := "secondary",
    version      := "2016.04.18",
    organization := "bdzimmer",
    scalaVersion := "2.10.6",

    mainClass in (Compile, run) := Some("bdzimmer.secondary.export.Driver"),

    javacOptions  ++= Seq("-source", jvmSettings.javacSource, "-target", jvmSettings.javacTarget),
    scalacOptions ++= Seq(s"-target:jvm-1.7"),

    libraryDependencies ++= Seq(
      "commons-io"         % "commons-io"                % "2.4",
      "org.apache.commons" % "commons-compress"          % "1.10",
      "org.pegdown"        % "pegdown"                   % "1.6.0",
      "com.fasterxml.jackson.core" % "jackson-core"      % "2.1.3",
      "org.scalatest"     %% "scalatest"                 % "2.2.4" % "it,test"
    ),

    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))

   )
  .configs(IntegrationTest)
  .settings(Defaults.itSettings: _*)
  .dependsOn(utilscala)
  .dependsOn(pixeleditor)
  .dependsOn(orbits)

lazy val utilscala   = RootProject(file("../util-scala"))
lazy val pixeleditor = RootProject(file("../pixel-editor"))
lazy val orbits      = RootProject(file("../orbits"))

// import into Eclipse as a Scala project
EclipseKeys.projectFlavor := EclipseProjectFlavor.Scala

// use Java 1.8 in Eclipse
EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE18)

// use the version of Scala from sbt in Eclipse
EclipseKeys.withBundledScalaContainers := false
