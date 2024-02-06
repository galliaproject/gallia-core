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
ThisBuild / libraryDependencies  += "com.lihaoyi" %% "utest" % uTestVersion % "test"
ThisBuild / testFrameworks       += new TestFramework("utest.runner.Framework")
// more tests: see https://github.com/galliaproject/gallia-testing (being moved here)

// ===========================================================================
lazy val reflect = (project in file("reflect"))
  .settings(
    name   := "gallia-reflect",
    target := baseDirectory.value / ".." / "bin" / "reflect" /* TODO: t240103170440 - still leaves a reflect/target folder somehow */)
  .settings(GalliaCommonSettings.mainSettings:_*)

// ---------------------------------------------------------------------------
lazy val core = (project in file("core"))
  .settings(
    name   := "gallia-core",
    target := baseDirectory.value / "bin" / "core")
  .settings(GalliaCommonSettings.mainSettings:_*)
  .dependsOn(reflect) // TODO: also bring in gallia-macros

// ---------------------------------------------------------------------------
lazy val root = (project in file("."))
  .settings(GalliaCommonSettings.mainSettings:_*)
  .aggregate(reflect, core)

// ===========================================================================    
// see https://github.com/aptusproject/aptus-core
//   our own utilities library, bundles low level library such as commons-{io,lang3,math3,csv}, gson, enumeratum, ...

lazy val aptusVersion      = "0.5.2"
lazy val enumeratumVersion = "1.7.3"
lazy val uTestVersion      = "0.8.1"

// ---------------------------------------------------------------------------
ThisBuild / libraryDependencies ++=
                   Seq("com.beachape" %% "enumeratum" % enumeratumVersion) ++
  (scalaBinaryVersion.value match {
    case "3"    => Seq(("io.github.aptusproject" %% "aptus-core" % aptusVersion).cross(CrossVersion.for3Use2_13) /* because of gallia-spark, which will bring in 2.13's scala-parallel-collections */)
    case "2.13" => Seq( "io.github.aptusproject" %% "aptus-core" % aptusVersion)
    case "2.12" => Seq( "io.github.aptusproject" %% "aptus-core" % aptusVersion) }) ++
  (scalaBinaryVersion.value match { // OSWO dependencies (see https://github.com/galliaproject/gallia-docs/blob/master/oswo.md)
    case "3"    => Seq() // TODO
    case "2.13" => Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value, "org.scala-lang" % "scala-library" % scalaVersion.value)
    case "2.12" => Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value, "org.scala-lang" % "scala-library" % scalaVersion.value) }) ++
  (scalaBinaryVersion.value match {
    case "3"    => Seq.empty
    case "2.13" => Seq("org.scala-lang" %  "scala-reflect" % scalaVersion.value) /* for scala.reflect.runtime.universe */
    case "2.12" => Seq("org.scala-lang" %  "scala-reflect" % scalaVersion.value) /* for scala.reflect.runtime.universe */ })                  

// ===========================================================================
sonatypeRepository     := "https://s01.oss.sonatype.org/service/local"
sonatypeCredentialHost :=         "s01.oss.sonatype.org"        
publishMavenStyle      := true
publishTo              := sonatypePublishToBundle.value

// ===========================================================================

