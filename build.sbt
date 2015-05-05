import sbt._

name := "ScalaImageToolbox"

version := "1.0"

scalaVersion in ThisBuild := "2.11.6"

organization := "org.kuleuven.mai"

lazy val root = (project in file(".")).aggregate(ActiveShapeModels,
  ImageDenoising, ModelReconstruction)

lazy val ActiveShapeModels = project in file("ActiveShapeModels")

lazy val ImageDenoising = project in file("ImageDenoising")

lazy val ModelReconstruction = (project in file("ModelReconstruction"))
  .dependsOn(ImageDenoising, ActiveShapeModels)

classpathTypes += "maven-plugin"

libraryDependencies in ThisBuild ++= Seq(
  "org.scalanlp" %% "breeze" % "0.11.2",
  "com.github.tototoshi" %% "scala-csv" % "1.2.1",
  "org.apache.logging.log4j" % "log4j-core" % "2.2")

resolvers += "Maven Central Server" at "http://repo1.maven.org/maven2"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

