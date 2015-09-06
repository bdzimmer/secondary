// Copyright (c) 2015 Ben Zimmer. All rights reserved.
// Secondary project sbt file

lazy val root = (project in file("."))
  .settings(
    name := "secondary",
    version := "2015.08.18",
    organization := "bdzimmer",
    scalaVersion := "2.10.5",
    
    javacOptions ++= Seq("-source", "1.7", "-target", "1.7"),
    
    libraryDependencies ++= Seq(
      "commons-io" % "commons-io" % "2.4",
      "org.apache.commons" % "commons-compress" % "1.10",
      "org.pegdown" % "pegdown" % "1.4.2",
      "org.yaml" % "snakeyaml" % "1.14",
      "com.google.apis" % "google-api-services-drive" % "v2-rev167-1.20.0",
      "org.scala-lang.modules" %% "scala-pickling" % "0.10.1",
      "org.scala-lang" % "scala-swing" % "2.10+",
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "net.liftweb" %% "lift-json" % "2.6"
    ))
    
  .dependsOn(gdrivescala)
  
    
lazy val gdrivescala = RootProject(file("../gdrive-scala"))
    
    
// import into Eclipse as a Scala project
EclipseKeys.projectFlavor := EclipseProjectFlavor.Scala

// use Java 1.7 in Eclipse    
EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17)

// use the version of Scala from sbt in Eclipse
EclipseKeys.withBundledScalaContainers := false