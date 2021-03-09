// gallia-core
//   trying to keep this to a mimimum
//   TODO: t210125110147 - investigate sbt alternatives

// ===========================================================================
lazy val root = (project in file("."))
  .settings(
    name               := "gallia-core",
    version            := "0.1.0",
    scalaVersion       := GalliaScalaVersions.supported.head,
    crossScalaVersions := GalliaScalaVersions.supported)

// ===========================================================================
// TODO:
// - more options
// - t210121165741: -Xdisable-assertions (also turns off require?)
lazy val galliaScalacOptions =
  Seq(
    "-encoding", "UTF-8",  
  //"-Yimports:java.lang,scala,scala.Predef,scala.util.chaining,aptus.Anything_" -- not possible for 2.12 it seems (TODO: t210308154253 confirm)
    "-Ywarn-value-discard")

// ---------------------------------------------------------------------------
scalacOptions in Compile ++= galliaScalacOptions ++ 
  (scalaBinaryVersion.value match {
    case "2.13" => Seq("-Ywarn-unused:imports")
    case _      => Seq("-Ywarn-unused-import" ) })

// ===========================================================================    
libraryDependencies ++=
  Seq(
    "org.scala-lang.modules" %% "scala-collection-compat" % GalliaCoreDependencyVersions.compatVersion, // to support scala <2.13

    // ---------------------------------------------------------------------------
    // misc utils
    "com.beachape"       %% "enumeratum"    % GalliaCoreDependencyVersions.enumeratumVersion withSources() withJavadoc(),
    "org.apache.commons" %  "commons-lang3" % GalliaCoreDependencyVersions.commonsVersion    withSources() withJavadoc(),
    "org.apache.commons" %  "commons-math3" % GalliaCoreDependencyVersions.commonsVersion    withSources() withJavadoc(),
    
    // compatibility issues (eg https://issues.apache.org/jira/browse/HADOOP-10961)... TODO: t210121165120: shade
    //"com.google.guava" %  "guava"         % GalliaCoreDependencyVersions.guavaVersion      withSources() withJavadoc(),

    // ---------------------------------------------------------------------------
    // XSV (tsv, csv, ...), compression (bz2, ...)
    "org.apache.commons" % "commons-csv"      % GalliaCoreDependencyVersions.commonsCsvVersion      withSources() withJavadoc(),    
    "org.apache.commons" % "commons-compress" % GalliaCoreDependencyVersions.commonsCompressVersion withSources() withJavadoc(),

    // ---------------------------------------------------------------------------
    // JSON
    "com.google.code.gson" % "gson" % GalliaCoreDependencyVersions.gsonVersion withSources() withJavadoc()) ++
  //    
  // ---------------------------------------------------------------------------
  (scalaBinaryVersion.value match {
    case "2.13" => Seq("org.scala-lang.modules" %% "scala-parallel-collections" % GalliaCoreDependencyVersions.parallelCollectionsVersion)
    case _      => Seq.empty })

// ===========================================================================

