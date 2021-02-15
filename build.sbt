// gallia-core
//   trying to keep this to a mimimum; TODO: t210125110147 - investigate sbt alternatives

// ===========================================================================
lazy val scala213 = "2.13.4"
lazy val scala212 = "2.12.13"
lazy val scala211 = "2.11.12"
lazy val scala210 = "2.10.7"

// ---------------------------------------------------------------------------
// errors with <2.13 with collection.mutable.ArrayDeque and SeqView[T, Seq[_]] (even with scala.collection.compat._)
//   see workaround toggle (210214150322): cat ./src/main/scala/cross/CrossPackage.scala | awk '/213/{sub(/^\/\//,"  ");print}/212/{sub(/^  /,"//");print}!/212/ && !/213/{print}'
lazy val supportedScalaVersions = List(scala213) // TODO: t210213101700 - scala212, scala211, scala210

// ===========================================================================
lazy val root = (project in file("."))
  .settings(
    name               := "gallia-core",
    version            := "0.1.0",
    scalaVersion       := supportedScalaVersions.head,
    crossScalaVersions := supportedScalaVersions)

// ===========================================================================
// TODO: more
scalacOptions in Compile ++=
  Seq("-Ywarn-value-discard") ++ 
  (scalaBinaryVersion.value match {
    case "2.13" => Seq("-Ywarn-unused:imports")
    case _      => Seq("-Ywarn-unused-import" ) })
// TODO: -Xdisable-assertions (also turns off require?)

// ===========================================================================
// TODO: to their own external object?

lazy val compatVersion          = "2.4.1"
lazy val enumeratumVersion      = "1.5.13" 
lazy val commonsVersion         = "3.5"
lazy val commonsCsvVersion      = "1.8"
lazy val commonsCompressVersion = "1.20"
//lazy val guavaVersion         = "28.0-jre"

// ---------------------------------------------------------------------------
lazy val junitVersion          = "4.12"
lazy val junitInterfaceVersion = "0.11" // see https://github.com/sbt/junit-interface

// ===========================================================================    
libraryDependencies ++=
  Seq(  
    "org.scala-lang"         %  "scala-reflect"           % scalaVersion.value, // else warning: "Multiple dependencies with the same organization/name but different versions"
    "org.scala-lang.modules" %% "scala-collection-compat" % compatVersion,      // to support scala <2.13

    // ---------------------------------------------------------------------------
    // misc utils
    "com.beachape"       %% "enumeratum"    % enumeratumVersion withSources() withJavadoc(),
    "org.apache.commons" %  "commons-lang3" % commonsVersion    withSources() withJavadoc(),
    "org.apache.commons" %  "commons-math3" % commonsVersion    withSources() withJavadoc(),
  //"com.google.guava"   %  "guava"         % guavaVersion      withSources() withJavadoc(), // compatibility issues (eg https://issues.apache.org/jira/browse/HADOOP-10961)... TODO: shade

    // ---------------------------------------------------------------------------
    // XSV (tsv, csv, ...), compression (bz2, ...)
    "org.apache.commons" % "commons-csv"      % commonsCsvVersion      withSources() withJavadoc(),    
    "org.apache.commons" % "commons-compress" % commonsCompressVersion withSources() withJavadoc(),

    // ---------------------------------------------------------------------------
    // JSON
    "com.google.code.gson" % "gson" % "2.8.6" withSources() withJavadoc(),

	// ===========================================================================
    // tests
    "junit"        % "junit"           % junitVersion          % "test" withSources() withJavadoc(),
    "com.novocode" % "junit-interface" % junitInterfaceVersion % "test" withSources() withJavadoc())

// ===========================================================================

