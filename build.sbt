// gallia-core
//   trying to keep this to a mimimum
//   TODO: t210125110147 - investigate sbt alternatives, especially https://github.com/com-lihaoyi/mill

// ===========================================================================
lazy val root = (project in file("."))
  .settings(
    organizationName     := "Gallia Project",
    organization         := "io.github.galliaproject", // *must* match groupId for sonatype
    name                 := "gallia-core",
    version              := "0.3.1",    
    homepage             := Some(url("https://github.com/galliaproject/gallia-core")),
    scmInfo              := Some(ScmInfo(
        browseUrl  = url("https://github.com/galliaproject/gallia-core"),
        connection =     "scm:git@github.com:galliaproject/gallia-core.git")),
    licenses             := Seq("BSL 1.1" -> url("https://github.com/galliaproject/gallia-core/blob/master/LICENSE")),
    description          := "A Scala library for data manipulation" )
  .settings(GalliaCommonSettings.mainSettings:_*)

// ===========================================================================    
// see https://github.com/aptusproject/aptus-core
//   our own utilities library, bundles low level library such as commons-{io,lang3,math3,csv}, gson, enumeratum, ...
lazy val aptusVersion      = "0.3.0"

lazy val enumeratumVersion = "1.5.13"

// ---------------------------------------------------------------------------
libraryDependencies ++=
  Seq(
    "org.scala-lang"         %  "scala-reflect" % scalaVersion.value, // for scala.reflect.runtime.universe
    "io.github.aptusproject" %% "aptus-core"    % aptusVersion,
    "com.beachape"           %% "enumeratum"    % enumeratumVersion)

// ---------------------------------------------------------------------------
// tests: see https://github.com/galliaproject/gallia-testing

// ===========================================================================
sonatypeRepository     := "https://s01.oss.sonatype.org/service/local"
sonatypeCredentialHost :=         "s01.oss.sonatype.org"        
publishMavenStyle      := true
publishTo              := sonatypePublishToBundle.value

// ===========================================================================

