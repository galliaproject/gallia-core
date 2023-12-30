// gallia-core
//   trying to keep this to a mimimum
//   TODO: t210125110147 - investigate sbt alternatives, especially https://github.com/com-lihaoyi/mill

// ===========================================================================
ThisBuild / organizationName     := "Gallia Project"
ThisBuild / organization         := "io.github.galliaproject" // *must* match groupId for sonatype
ThisBuild / organizationHomepage := Some(url("https://github.com/galliaproject"))
ThisBuild / startYear            := Some(2021)
ThisBuild / version              := "0.6.0-SNAPSHOT"
ThisBuild / description          := "A Scala library for data manipulation"
ThisBuild / homepage             := Some(url("https://github.com/galliaproject/gallia-core"))
ThisBuild / licenses             := Seq("Apache 2" -> url("https://github.com/galliaproject/gallia-core/blob/master/LICENSE"))
ThisBuild / developers           := List(Developer(
  id    = "anthony-cros",
  name  = "Anthony Cros",
  email = "contact.galliaproject@gmail.com",
  url   = url("https://github.com/anthony-cros")))
ThisBuild / scmInfo              := Some(ScmInfo(
  browseUrl  = url("https://github.com/galliaproject/gallia-core"),
  connection =     "scm:git@github.com:galliaproject/gallia-core.git"))

// ===========================================================================
lazy val reflect = (project in file("reflect"))
  .settings(
    name   := "gallia-reflect",
    target := baseDirectory.value / ".." / "bin" / "reflect")
  .settings(GalliaCommonSettings.mainSettings:_*)


// ---------------------------------------------------------------------------
// TODO: t231230123606 - how to nest core under its own folder? tests no longer found when trying
lazy val root = (project in file("."))
  .settings(
    name   := "gallia-core",
    target := baseDirectory.value / "bin" / "core")
  .settings(GalliaCommonSettings.mainSettings:_*)
  .dependsOn(reflect) // TODO: also bring in gallia-macros

// ===========================================================================    
// see https://github.com/aptusproject/aptus-core
//   our own utilities library, bundles low level library such as commons-{io,lang3,math3,csv}, gson, enumeratum, ...

lazy val aptusVersion      = "0.5.2"
lazy val enumeratumVersion = "1.7.3"
lazy val uTestVersion      = "0.8.1"

// ---------------------------------------------------------------------------
ThisBuild / libraryDependencies ++=
  Seq(    
    "io.github.aptusproject" %% "aptus-core"    % aptusVersion,
    "com.beachape"           %% "enumeratum"    % enumeratumVersion) ++
  (scalaBinaryVersion.value match {
    case "3"    => Seq.empty
    case "2.13" => Seq("org.scala-lang" %  "scala-reflect" % scalaVersion.value) /* for scala.reflect.runtime.universe */
    case "2.12" => Seq("org.scala-lang" %  "scala-reflect" % scalaVersion.value) /* for scala.reflect.runtime.universe */ })         

// ===========================================================================
// testing

libraryDependencies += "com.lihaoyi" %% "utest" % uTestVersion % "test" withSources() withJavadoc()

testFrameworks += new TestFramework("utest.runner.Framework")

// ---------------------------------------------------------------------------
// more tests: see https://github.com/galliaproject/gallia-testing

// ===========================================================================
sonatypeRepository     := "https://s01.oss.sonatype.org/service/local"
sonatypeCredentialHost :=         "s01.oss.sonatype.org"        
publishMavenStyle      := true
publishTo              := sonatypePublishToBundle.value

// ===========================================================================

