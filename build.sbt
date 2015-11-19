// Copyright (c) 2015 Ben Zimmer. All rights reserved.

// Secondary project build.sbt file

val whichJvmSettings = sys.props.getOrElse("jvm", default = "7")
val jvmSettings = whichJvmSettings match {
  case "6" => JvmSettings("1.6", "1.6", "1.6")
  case _ => JvmSettings("1.7", "1.7", "1.7")
}

// JVM settings can be verified using the following command:
// javap -verbose -cp secondary.jar bdzimmer.secondary.export.Driver
// major version will be 50 for Java 1.6 and 51 for Java 1.7.

lazy val root = (project in file("."))
  .settings(
    name := "Secondary",
    version := "2015.11.13",
    organization := "bdzimmer",
    scalaVersion := "2.10.6",
    mainClass in (Compile, run) := Some("bdzimmer.secondary.export.Driver"),

    javacOptions ++= Seq("-source", jvmSettings.javacSource, "-target", jvmSettings.javacTarget),
    scalacOptions ++= Seq(s"-target:jvm-${jvmSettings.scalacTarget}"),

    libraryDependencies ++= Seq(
      "commons-io" % "commons-io" % "2.4",
      "org.apache.commons" % "commons-compress" % "1.10",
      "org.pegdown" % "pegdown" % "1.6.0",
      "org.yaml" % "snakeyaml" % "1.14",
      "com.google.apis" % "google-api-services-drive" % "v2-rev167-1.20.0",
      "org.scala-lang" % "scala-swing" % "2.10+",
      "org.scalatest" %% "scalatest" % "2.2.4" % "it,test",
      "net.liftweb" %% "lift-json" % "2.6"
    ))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings: _*)
  .dependsOn(gdrivescala)


lazy val gdrivescala = RootProject(file("../gdrive-scala"))


// import into Eclipse as a Scala project
EclipseKeys.projectFlavor := EclipseProjectFlavor.Scala

// use Java 1.7 in Eclipse
EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17)

// use the version of Scala from sbt in Eclipse
EclipseKeys.withBundledScalaContainers := false
