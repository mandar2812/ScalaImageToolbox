import sbt._

name := "ScalaImageToolbox"

version := "1.0"

scalaVersion in ThisBuild := "2.11.6"

organization := "org.kuleuven.mai"

classpathTypes += "maven-plugin"

val breezeVersion = "0.11.2"

val javacppVersion = "0.11"

lazy val root = (project in file(".")).aggregate(ActiveShapeModels,
  ImageDenoising, ModelReconstruction)

lazy val ActiveShapeModels = project in file("ActiveShapeModels")

lazy val ImageDenoising = project in file("ImageDenoising")

lazy val ModelReconstruction = (project in file("ModelReconstruction"))
  .dependsOn(ImageDenoising, ActiveShapeModels)

val platform = {
  // Determine platform name using code similar to javacpp
  // com.googlecode.javacpp.Loader.java line 60-84
  val jvmName = System.getProperty("java.vm.name").toLowerCase
  var osName = System.getProperty("os.name").toLowerCase
  var osArch = System.getProperty("os.arch").toLowerCase
  if (jvmName.startsWith("dalvik") && osName.startsWith("linux")) {
    osName = "android"
  } else if (jvmName.startsWith("robovm") && osName.startsWith("darwin")) {
    osName = "ios"
    osArch = "arm"
  } else if (osName.startsWith("mac os x")) {
    osName = "macosx"
  } else {
    val spaceIndex = osName.indexOf(' ')
    if (spaceIndex > 0) {
      osName = osName.substring(0, spaceIndex)
    }
  }
  if (osArch.equals("i386") || osArch.equals("i486") || osArch.equals("i586") || osArch.equals("i686")) {
    osArch = "x86"
  } else if (osArch.equals("amd64") || osArch.equals("x86-64") || osArch.equals("x64")) {
    osArch = "x86_64"
  } else if (osArch.startsWith("arm")) {
    osArch = "arm"
  }
  val platformName = osName + "-" + osArch
  println("platform: " + platformName)
  platformName
}

libraryDependencies in ThisBuild ++= Seq(
  "org.scalanlp" %% "breeze" % breezeVersion,
  //"org.scalanlp" %% "breeze-natives" % breezeVersion,
  "com.github.tototoshi" %% "scala-csv" % "1.2.1",
  "org.apache.logging.log4j" % "log4j-core" % "2.2",
  "org.apache.spark" %% "spark-core" % "1.3.1",
  "org.apache.spark" %% "spark-mllib" % "1.3.1",
  /*"org.bytedeco"                 % "javacpp" % javacppVersion,
  "org.bytedeco"                 % "javacv" % javacppVersion,
  "org.bytedeco.javacpp-presets" % "opencv" % ("2.4.11-" + javacppVersion) classifier "",
  "org.bytedeco.javacpp-presets" % "opencv" % ("2.4.11-" + javacppVersion) classifier platform,*/
  "xerces" % "xercesImpl" % "2.8.1",
  "com.sksamuel.scrimage" %% "scrimage-core" % "1.4.2",
  "com.sksamuel.scrimage" %% "scrimage-canvas" % "1.4.2",
  "com.sksamuel.scrimage" %% "scrimage-filters" % "1.4.2",
  "org.scalaz" %% "scalaz-core" % "7.1.2"
)

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Maven Central Server" at "http://repo1.maven.org/maven2",
  /*"Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",*/
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

javaOptions in ThisBuild ++= Seq("-Dplatform.dependencies=true", "-Xmx2G")

