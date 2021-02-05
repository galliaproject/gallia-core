// gallia-core
//   trying to keep this to a mimimum; TODO: t210125110147 - investigate sbt alternatives

// ===========================================================================
lazy val root = (project in file("."))
  .settings(
    name         := "gallia-core",
    version      := "0.1.0",
    scalaVersion := "2.12.13")

// ===========================================================================
scalacOptions in Compile ++= Seq( // TODO: more + inherit
  "-Ywarn-value-discard",
  "-Ywarn-unused-import")

// ===========================================================================
// TODO: to their own external object?
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
    // else warning: Multiple dependencies with the same organization/name but different versions. To avoid conflict, pick one version: [warn]  * org.scala-lang:scala-reflect:(2.12.2, 2.12.4)
    "org.scala-lang" % "scala-reflect" % "2.12.13", // TODO: reuse scala version above

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
// build
// TODO: -Xdisable-assertions (also turns off require?)

// ===========================================================================

