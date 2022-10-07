import sbt     ._
import sbt.Keys._

// ===========================================================================
object GalliaCommonSettings {
  val CurrentGalliaVersion = "0.4.0"

  // ---------------------------------------------------------------------------
  val mainSettings = Seq(
    organizationHomepage := Some(url("https://github.com/galliaproject")),
    startYear            := Some(2021),
    developers           :=
      List(Developer(
        id    = "anthony-cros",
        name  = "Anthony Cros",
        email = "contact.galliaproject@gmail.com",
        url   = url("https://github.com/anthony-cros") )),

    // ---------------------------------------------------------------------------
         scalaVersion  := GalliaScalaVersions.supported.head,
    crossScalaVersions := GalliaScalaVersions.supported)

  // ===========================================================================
  // TODO: t210121165741: -Xdisable-assertions (also turns off require?)  
  scalacOptions ++= Seq(
        "-encoding", "UTF-8",  
      //"-Yimports:java.lang,scala,scala.Predef,scala.util.chaining,aptus.Anything_" -- not possible for 2.12 it seems (TODO: t210308154253 confirm)
        "-Ywarn-value-discard") ++ 
      // ---------------------------------------------------------------------------
      (scalaBinaryVersion.value match {
        case "2.13" => Seq("-Ywarn-unused:imports")
        case _      => Seq("-Ywarn-unused-import" ) })

}

// ===========================================================================

