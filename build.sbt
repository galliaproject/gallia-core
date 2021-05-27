// gallia-core
//   trying to keep this to a mimimum
//   TODO: t210125110147 - investigate sbt alternatives, especially https://github.com/com-lihaoyi/mill

// ===========================================================================
lazy val root = (project in file("."))
  .settings(
    name    := "gallia-core",
    version := "0.1.0")
  .settings(GalliaCommonSettings.mainSettings:_*)

// ===========================================================================    
// see https://github.com/aptusproject/aptus-core
//   our own utilities library, bundles low level library such as commons-{io,lang3,math3,csv}, gson, enumeratum, ...
libraryDependencies += "io.github.aptusproject" %% "aptus-core" % "0.1.0"

// ---------------------------------------------------------------------------
// tests: see https://github.com/galliaproject/gallia-testing

// ===========================================================================

