import sbt.Keys._

// ===========================================================================
object GalliaCommonSettings {

  // TODO:
  // - add organization (once established)
  // - t210121165741: -Xdisable-assertions (also turns off require?)
  val mainSettings = Seq(
    scalaVersion       := GalliaScalaVersions.supported.head,
    crossScalaVersions := GalliaScalaVersions.supported)
    
    // ---------------------------------------------------------------------------    
    scalacOptions      ++= Seq(
        "-encoding", "UTF-8",  
      //"-Yimports:java.lang,scala,scala.Predef,scala.util.chaining,aptus.Anything_" -- not possible for 2.12 it seems (TODO: t210308154253 confirm)
        "-Ywarn-value-discard") ++ 
      // ---------------------------------------------------------------------------
      (scalaBinaryVersion.value match {
        case "2.13" => Seq("-Ywarn-unused:imports")
        case _      => Seq("-Ywarn-unused-import" ) })

}

// ===========================================================================

